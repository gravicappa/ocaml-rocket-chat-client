module Config = struct
  type t = {
    username: string;
    password: string;
    server: string;
  }
  [@@deriving of_yojson { strict = false }]

  let create ~username ~password ~server = { username; password; server }
end

type error = [ `Msg of string]

module Session = struct
  type t = {
    id: string;
    token: string;
    server: string;
    mutable valid: bool;
  }
end

let api_url server addr = Printf.sprintf "%s/api/v1/%s" server addr

let ( let*? ) = Lwt_result.bind

let request ?(headers = []) ?body method_ url =
  let read _ buffer string =
    Buffer.add_string buffer string;
    Lwt.return buffer in

  match%lwt
    Http_lwt_client.request ~headers
                            ~meth: method_
                            ?body
                            url
                            read
                            (Buffer.create 0)
  with
  | Ok (resp, buffer) when Http_lwt_client.Status.is_successful resp.status ->
      begin match buffer |> Buffer.contents |> Yojson.Safe.from_string with
      | yojson -> Lwt.return_ok yojson
      | exception Yojson.Json_error e -> Lwt.return_error (`Msg e)
      end
  | Ok (resp, buffer) ->
      let error = Format.asprintf "%a:%s:%s"
                                  Http_lwt_client.Status.pp_hum resp.status
                                  resp.reason
                                  (Buffer.contents buffer) in
      Lwt.return_error (`Msg error)
  | Error _ as e -> Lwt.return e

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

  let perform config =
    let body =
      { username = config.Config.username; password = config.Config.password; }
      |> request_to_yojson
      |> Yojson.Safe.to_string in

    let*? yojson = request `POST ~body (api_url config.Config.server "login") in
    match response_of_yojson yojson with
    | Ok { status = "success"; data = Some cred } -> Lwt.return_ok cred
    | Ok _ -> Lwt.return_error (`Msg "unknown-error")
    | Error err -> Lwt.return_error (`Msg err)
end

let url { Session.server; _ } addr = api_url server addr

let add_headers { Session.id; token; _ } headers =
  ("X-User-Id", id) :: ("X-Auth-Token", token) :: headers

let get s addr =
  let headers = add_headers s [] in
  request ~headers `GET (url s addr)

let post s addr json =
  let headers = add_headers s [] in
  let url = url s addr in
  let body = Yojson.Safe.to_string json in
  request ~headers `POST ~body url

let logout s =
  match s.Session.valid with
  | true ->
      let%lwt _ = post s "logout" `Null in
      Lwt.return_unit
  | false -> Lwt.return_unit

let login c =
  let*? { Login.id; token } = Login.perform c in
  Lwt.return_ok Session.{ id; token; server = c.server; valid = true }

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
    let*? yojson = get s "groups.list" in
    match groups_of_yojson yojson with
    | Ok _ as ok -> Lwt.return ok
    | Error error -> Lwt.return_error (`Msg error)
end

module Chat = struct
  type message = {
    channel: string;
    text: string;
    alias: string option;
  }
  [@@deriving yojson { strict = false }]

  let create ?alias ~channel text = { channel; text; alias }

  let send s m =
    message_to_yojson m
    |> post s "chat.postMessage"
    |> ignore_result
end
