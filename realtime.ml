module WS = Websocket_client
module Settings = Websocket_client.Settings

type rocket_chat = {
  channel: WS.t;
}

type t = rocket_chat

type error = [`Msg of string]

let trace = ref (fun _ -> ())

let send_yojson channel yojson =
  let str = Yojson.Safe.to_string yojson in
  WS.send_text channel (str ^ "\n")

let next_id =
  let count = ref 0L in
  fun () ->
    let n = !count in
    count := Int64.add n 1L;
    Printf.sprintf "%Lx" n

module Header = struct
  type t = {
    id: string [@default ""];
    msg: string;
  }
  [@@deriving yojson { strict = false }]

  let create msg = { id = next_id (); msg }
end

module Error = struct
  type t = {
    error: int;
    reason: string;
    message: string;
    type_: string [@key "errorType"];
  }
  [@@deriving of_yojson { strict = false }]
end

module Response = struct
  type result = {
    result: Yojson.Safe.t;
  }
  [@@deriving of_yojson { strict = false }]

  type error = {
    error: Error.t
  }
  [@@deriving of_yojson { strict = false }]

  type t =
    | Value of Yojson.Safe.t
    | Error of string
    | End

  let res parse a =
    match%lwt a with
    | Value v ->
        begin match parse v with
        | Ok _ as ok-> Lwt.return ok
        | Error error -> Lwt.return_error (`Msg error)
        end
    | Error error -> Lwt.return_error (`Msg error)
    | End -> Lwt.return_error (`Msg "end-of-stream")
end

let requests = Hashtbl.create 256

module Method = struct
  type t = {
    name: string [@key "method"];
    params: Yojson.Safe.t array;
  }
  [@@deriving yojson { strict = false }]

  let send channel hdr name params =
    to_yojson { name; params }
    |> Yojson.Safe.Util.combine (Header.to_yojson hdr)
    |> send_yojson channel

  let call { channel; _ } name params =
    if WS.is_open channel then
      let cond = Lwt_condition.create () in
      let hdr = Header.create "method" in
      Hashtbl.replace requests hdr.id cond;
      send channel hdr name params;
      Lwt_condition.wait cond
    else
      Lwt.return Response.End
end

module Connect = struct
  type t = {
    msg: string;
    version: string;
    support: string array;
  }
  [@@deriving yojson { strict = false }]

  let id = "connected"

  type connected = {
    msg: string;
    session: string;
  }
  [@@deriving of_yojson { strict = false }]

  let message = { msg = "connect"; version = "1"; support = [| "1" |] }

  let connect channel =
    if WS.is_open channel then
      let cond = Lwt_condition.create () in
      Hashtbl.replace requests id cond;
      message
      |> to_yojson
      |> send_yojson channel;
      let%lwt res = Lwt_condition.wait cond in
      match res with
      | Response.Value _ -> Lwt.return_ok ()
      | Error error -> Lwt.return_error (`Msg error)
      | End -> Lwt.return_error (`Msg "end-of-stream")
    else
      Lwt.return_error (`Msg "end-of-stream")
end

module Subscription = struct
  type recipient =
    | Me
    | Room of string

  type message = {
    id: string;
    recipient: recipient;
    where: string;
    from: string;
    text: string;
    timestamp: int;
  }

  type event =
    | Message of message
    | End

  type t = {
    id: recipient;
    cond: event Lwt_condition.t;
    t: rocket_chat;
  }

  type request = {
    name: string;
    params: Yojson.Safe.t array;
  }
  [@@deriving yojson { strict = false }]

  module Response = struct
    type from = {
      username: string;
    }
    [@@deriving of_yojson { strict = false }]

    type timestamp = {
      date: int [@key "$date"];
    }
    [@@deriving of_yojson { strict = false }]

    type arg = {
      id: string [@key "_id"];
      msg: string;
      u: from;
      ts: timestamp;
      rid: string;
      t: string option [@default None];
      reactions: Yojson.Safe.t option [@default None];
      replies: Yojson.Safe.t option [@default None];
    }
    [@@deriving of_yojson { strict = false }]

    type field = {
      event_name: string [@key "eventName"];
      args: Yojson.Safe.t list;
    }
    [@@deriving of_yojson { strict = false }]

    type t = {
      fields: field;
    }
    [@@deriving of_yojson { strict = false }]
  end

  let requests = Hashtbl.create 256

  let self_id = "__my_messages__"

  let recipient_of_string = function
    | id when id = self_id -> Me
    | id -> Room id

  let string_of_recipient = function
    | Me -> self_id
    | Room id -> id

  let rec map_response recipient args proc =
    match args with
      | [] -> ()
      | a :: rest ->
          !trace (Yojson.Safe.to_string (`Assoc ["<< response.arg", a]));
          match Response.arg_of_yojson a with
          | Ok ({ reactions = None; replies = None; _ } as response) ->
              proc {
                id = response.id;
                recipient;
                from = response.u.username;
                text = response.msg;
                timestamp = response.ts.date;
                where = response.rid;
              };
              map_response recipient rest proc
          | Ok _ -> map_response recipient rest proc
          | Error _ -> map_response recipient rest proc

  let dispatch yojson =
    match Response.of_yojson yojson with
    | Error _ -> Lwt.return_unit
    | Ok { fields = { event_name; args } } ->
        let recipient = recipient_of_string event_name in
        map_response recipient args (fun message ->
          match Hashtbl.find_opt requests message.recipient with
          | None -> ()
          | Some cond -> Lwt_condition.broadcast cond (Message message));
        Lwt.return_unit

  let close_all_listeners () =
    Hashtbl.to_seq_values requests
    |> Seq.iter (fun cond -> Lwt_condition.broadcast cond End)

  let send channel hdr name params =
    request_to_yojson { name; params }
    |> Yojson.Safe.Util.combine (Header.to_yojson hdr)
    |> send_yojson channel

  let call channel msg name params =
    let cond = Lwt_condition.create () in
    let hdr = Header.create msg in
    send channel hdr name params;
    cond

  let subscribe t id name params =
    let cond = call t.channel "sub" name params in
    Hashtbl.replace requests id cond;
    { id; cond; t }

  let unsubscribe { id; t; _ } =
    Hashtbl.remove requests id;
    send t.channel (Header.create "unsub") (string_of_recipient id) [| |]

  let listen proc sub =
    let rec loop proc sub =
      match%lwt Lwt_condition.wait sub.cond with
      | Message message ->
          let%lwt () = proc sub message in
          loop proc sub
      | End -> Lwt.return_unit in

    Lwt.finalize (fun () -> loop proc sub)
                 (fun () ->
                   unsubscribe sub;
                   Lwt.return_unit)

  let stream_room_messages channel id =
    [| `String (string_of_recipient id); `Bool false |]
    |> subscribe channel id "stream-room-messages"
end

module Room = struct
  type type_ =
    | Public
    | Direct
    | Unknown

  let type__of_yojson = function
    | `String "d" -> Ok Direct
    | `String "p" -> Ok Public
    | _ -> Ok Unknown

  let type__to_yojson = function
    | Public -> `String "p"
    | Direct -> `String "d"
    | Unknown -> `Null

  type t = {
    id: string [@key "_id"];
    name: string option [@default None];
    type_: type_ [@key "t"];
    usernames: string list [@default []];
  }
  [@@deriving yojson { strict = false }]

  type resp = {
    update: t array;
    remove: string array;
  }
  [@@deriving of_yojson { strict = false }]

  let available_rooms channel =
    match%lwt
      Method.call channel "rooms/get" [| `Assoc ["$date", `Int 0] |]
      |> Response.res resp_of_yojson
    with
    | Ok { update; _ } -> Lwt.return_ok update
    | Error error -> Lwt.return_error error

  let id_of_name t room =
    match%lwt available_rooms t with
    | Ok rooms ->
        let n = Array.length rooms in
        let rec find i = 
          if i < n then
            match rooms.(i) with
            | { id; name = Some name; type_ = Public; _ } when name = room ->
                Lwt.return_some id
            | _ -> find (i + 1)
          else Lwt.return_none in
        find 0
    | Error _ -> Lwt.return_none
end

module Login = struct
  type user = {
    username: string;
  }
  [@@deriving yojson { strict = false }]

  type password = {
    digest: string;
    algorithm: string;
  }
  [@@deriving yojson { strict = false }]

  type request = {
    user: user;
    password: password;
  }
  [@@deriving yojson { strict = false }]

  type t = {
    id: string;
    token: string;
  }
  [@@deriving yojson { strict = false }]

  let sha256_hex str =
    str
    |> Digestif.SHA256.digest_string
    |> Digestif.SHA256.to_hex

  let login t ~username ~password =
    let param = {
      user = { username };
      password = {
        digest = sha256_hex password;
        algorithm = "sha-256"
      };
    } in

    Method.call t "login" [| request_to_yojson param |]
    |> Response.res of_yojson

  let logout t =
    let%lwt _ = Method.call t "logout" [| |] in
    Lwt.return_unit

  let with_login rc ~username ~password proc =
    match%lwt login rc ~username ~password with
    | Ok _ as resp ->
        Lwt.finalize (fun () -> proc resp)
                     (fun () -> logout rc)
    | Error _ as resp -> proc resp
end

module Message = struct
  type param = {
    id: string [@key "_id"];
    room: string [@key "rid"];
    text: string [@key "msg"];
    alias: string option [@default None];
    groupable: bool [@default true];
  }
  [@@deriving to_yojson]

  let param_of room text alias =
    let t = Unix.time () |> int_of_float in
    let id = Printf.sprintf "message:%016x:%s" t (next_id ()) in
    ignore alias; (* ignore for now *)
    { id; room; text; alias = None; groupable = true }

  let send rc ?alias ~room text =
    let param = param_of room text alias |> param_to_yojson in
    let%lwt _ = Method.call rc "sendMessage" [| param |] in
    Lwt.return_unit
end

let process_text_message channel text =
  let handle_response cond yojson =
    match Response.error_of_yojson yojson with
    | Ok { error } ->
        Lwt_condition.broadcast cond (Response.Error error.reason)
    | Error _ ->
        match Response.result_of_yojson yojson with
        | Ok { result } -> Lwt_condition.broadcast cond (Response.Value result)
        | Error error -> Lwt_condition.broadcast cond (Response.Error error) in

  let handle_connected cond yojson =
    Lwt_condition.broadcast cond (Response.Value yojson) in

  let dispatch_by_id id yojson proc =
    match Hashtbl.find_opt requests id with
    | None -> Lwt.return_unit
    | Some cond ->
        Hashtbl.remove requests id;
        proc cond yojson;
        Lwt.return_unit in

  let pong channel =
    `Assoc ["msg", `String "pong"]
    |> send_yojson channel in

  !trace text;
  let yojson = Yojson.Safe.from_string text in
  match Header.of_yojson yojson with
  | Error _ -> Lwt.return_unit
  | Ok { msg = "ping"; id = "" } ->
      pong channel;
      Lwt.return_unit
  | Ok { msg = "connected"; id = "" } ->
      dispatch_by_id Connect.id yojson handle_connected
  | Ok { msg = "changed"; _ } -> Subscription.dispatch yojson
  | Ok { id; _ } -> dispatch_by_id id yojson handle_response

let close_all_listeners () =
  Hashtbl.to_seq_values requests
  |> Seq.iter (fun cond -> Lwt_condition.broadcast cond Response.End);
  Subscription.close_all_listeners ()

let transform_uri uri =
  let to_websock u scheme =
    let u = Uri.with_scheme u (Some scheme) in
    Uri.with_path u "/websocket" in

  let transform uri = 
    match Uri.scheme uri with
    | Some "ws"
    | Some "wss" -> uri
    | Some "http" -> to_websock uri "ws"
    | _ -> to_websock uri "wss" in

  uri
  |> Uri.of_string
  |> transform
  |> Uri.to_string

let create ~settings uri =
  let process channel = function
    | WS.Response.Text text -> process_text_message channel text
    | WS.Response.Binary _ -> Lwt.return_unit
    | WS.Response.Error _
    | WS.Response.End ->
        close_all_listeners ();
        WS.close channel;
        Lwt.return_unit in

  let uri = transform_uri uri in
  match%lwt WS.create ~settings uri process with
  | Error _ as error -> Lwt.return error
  | Ok channel ->
      match%lwt Connect.connect channel with
      | Ok () -> Lwt.return_ok { channel }
      | Error error -> Lwt.return_error error

let close rc =
  let%lwt () = Login.logout rc in
  close_all_listeners ();
  Lwt.return_unit

let with_chat ~settings uri proc =
  match%lwt create ~settings uri with
  | Ok rc as r ->
      Lwt.finalize (fun () -> proc r)
                   (fun () -> close rc)
  | Error _ as r -> proc r
