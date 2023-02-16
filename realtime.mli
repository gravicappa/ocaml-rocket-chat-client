(** Implementation of realtime client for Rocket.Chat *)

(** Connection settings *)
module Settings: module type of Websocket_client.Settings

type rocket_chat
type t = rocket_chat

(** Subscription management *)
module Subscription: sig
  type recipient = Me | Room of string

  type t

  type message = {
    recipient: recipient;
    where: string;
    from: string;
    text: string;
    timestamp: int64;
  }
  
  val unsubscribe: t -> unit

  (** [listen proc t] listens a subscription [t] for incoming messages in a
      loop. Calls [proc] for each of them *)
  val listen: (t -> message -> unit Lwt.t) -> t -> unit Lwt.t

  (** Subscribe to given messages. *)
  val stream_room_messages: rocket_chat -> recipient -> t
end

module Room : sig
  type type_ = Public | Direct | Unknown

  type t = {
    id : string;
    name : string option;
    type_ : type_;
    usernames : string list;
  }

  (** Return an array of rooms available for current user *)
  val available_rooms : rocket_chat -> (t array, string) Result.result Lwt.t

  (** Get ID of a room by name *)
  val id_of_name : rocket_chat -> string -> string option Lwt.t
end

module Login: sig
  type t = {
    id: string;
    token: string;
  }

  val login :
    rocket_chat ->
    username:string ->
    password:string -> (t, string) Result.result Lwt.t

  val logout : rocket_chat -> unit Lwt.t

  (** [with_login rc ~username ~password proc] performs [login] with given
      credentials ([username], [password]) then calls [proc]. If login was
      successful it calls [logout] after [proc]. *)
  val with_login :
    rocket_chat ->
    username:string ->
    password:string -> ((t, string) result -> 'a Lwt.t) -> 'a Lwt.t
end

module Message: sig
  (** [send rc ~alias room text] sends [text] to [room] under [alias] (if
      specified). *)
  val send: rocket_chat -> ?alias:string -> room:string -> string -> unit Lwt.t
end

val create :
  settings:Settings.t ->
  string -> (rocket_chat, string) Result.result Lwt.t

val close : rocket_chat -> unit Lwt.t

val with_chat :
  settings:Settings.t ->
  string -> ((rocket_chat, string) result -> 'a Lwt.t) -> 'a Lwt.t

val trace: (string -> unit) ref
