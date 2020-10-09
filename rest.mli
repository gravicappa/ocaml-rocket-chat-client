(** Implementation of REST client for Rocket.Chat *)

(** Configuration *)
module Config : sig
  type t = {
    username: string;
    password: string;
    server: string; (* server url *)
  }

  val of_yojson : Yojson.Safe.t -> (t, string) result

  val create : username:string -> password:string -> server:string -> t
end

module Session: sig
  type t
end

val logout: Session.t -> unit Lwt.t

val login: Config.t -> (Session.t, string) Result.result Lwt.t

module Group: sig
  type t = {
    id: string;
    name: string;
  }

  type groups = {
    groups: t list;
  }

  val list : Session.t -> (groups, string) Result.result Lwt.t
end

module Chat: sig
  type message = {
    channel : string;
    text : string;
    alias : string option;
  }
  val create: ?alias:string -> channel:string -> text:string -> message
  val send: Session.t -> message -> (unit, string) Result.result Lwt.t
end
