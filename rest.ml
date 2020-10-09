module Config = struct
  type t = {
    username: string;
    password: string;
    server: string;
  }
  [@@deriving of_yojson { strict = false }]

  let create ~username ~password ~server = { username; password; server }
end

let receive proc response =
  match%lwt response with
  | Ok (_, json) -> Yojson.Safe.from_string json |> proc
  | Error (code, err) ->
      Printf.sprintf "HTTP: %d: %s" code err
      |> Lwt.return_error

module Session = struct
  type t = {
    id: string;
    token: string;
    server: string;
    mutable valid: bool;
  }
end

let api_url server addr = Printf.sprintf "%s/api/v1/%s" server addr

module Login = struct
  type request = {
    username: string;
    password: string;
  }
  [@@deriving to_yojson]

  type credentials = {
    id: string [@key "userId"];
    token: string [@key "authToken"];
  }
  [@@deriving of_yojson { strict = false }]

  type response = {
    status: string;
    data: credentials option;
  }
  [@@deriving of_yojson]

  let process yojson =
    match response_of_yojson yojson with
    | Ok { status = "success"; data = Some cred } -> Lwt.return_ok cred
    | Ok _ -> Lwt.return_error "Unknown"
    | Error err -> Lwt.return_error err

  let perform config =
    { username = config.Config.username; password = config.Config.password; }
    |> request_to_yojson
    |> Yojson.Safe.to_string
    |> Rest_client.post (api_url config.Config.server "login")
    |> receive process
end

let url { Session.server; _ } addr = api_url server addr

let add_headers { Session.id; token; _ } headers =
  ("X-User-Id", id) :: ("X-Auth-Token", token) :: headers

let setup_curl c =
  Curl.set_verbose c false

let settings = { Rest_client.default_settings with setup = setup_curl }

let get s addr proc =
  let headers = add_headers s [] in
  let url = url s addr in
  Rest_client.get ~settings ~headers url
  |> receive proc

let post s addr json =
  let headers = add_headers s [] in
  let url = url s addr in
  let data = Yojson.Safe.to_string json in
  Rest_client.post ~settings ~headers url data
  |> receive Lwt.return_ok

let logout s =
  match s.Session.valid with
  | true ->
      let%lwt _ = post s "logout" `Null in
      Lwt.return_unit
  | false -> Lwt.return_unit

let login c =
  match%lwt Login.perform c with
  | Ok { Login.id; token } ->
      let s = Session.{ id; token; server = c.server; valid = true } in
      Lwt_main.at_exit (fun () -> logout s);
      Lwt.return_ok s
  | Error err ->
      Lwt.return_error err

let ignore_result resp =
  match%lwt resp with
  | Ok _ -> Lwt.return_ok ()
  | Error err -> Lwt.return_error err

module Group = struct
  type t = {
    id: string [@key "_id"];
    name: string [@key "fname"];
  }
  [@@deriving yojson { strict = false }]

  type groups = {
    groups: t list;
  }
  [@@deriving yojson { strict = false }]

  let list s =
    get s "groups.list" @@ fun a ->
      groups_of_yojson a |> Lwt.return
end

module Chat = struct
  type message = {
    channel: string;
    text: string;
    alias: string option;
  }
  [@@deriving yojson { strict = false }]

  let create ?alias ~channel ~text = { channel; text; alias }

  let send s m =
    message_to_yojson m
    |> post s "chat.postMessage"
    |> ignore_result
end
