(** Async compatible client to the beanstalkd work queue *)

open Core.Std
open Async.Std

type conn

val connect : host:string -> port:int -> conn Deferred.t

val default_connection : ?host:string -> ?port:int -> unit -> conn Deferred.t

val quit : conn -> unit Deferred.t

module Job : sig
  type t = private {
    id : int;
    data : string;
  } with sexp
  val id : t -> int
  val data : t -> string
end

module Tube : sig
  (** common operations *)
  val all : conn -> string list Deferred.t
  val stats : conn -> tube:string -> (string * string) list Deferred.t
  val pause : conn -> tube:string -> delay:int -> unit Deferred.t
  (** consumers *)
  val watched : conn -> string list Deferred.t
  val watch : conn -> tube:string -> [ `Watching of int ] Deferred.t
  val ignore_tube : conn -> tube:string -> unit Deferred.t
  (** producers *)
  val use : conn -> tube:string -> unit Deferred.t
  val using : conn -> [ `Tube of string ] Deferred.t
end

module Worker : sig
  val reserve : ?timeout:int -> conn -> Job.t Deferred.t
  (** job operations *)
  val put : conn -> ?delay:int ->
    priority:int -> ttr:int -> data:string -> Job.t Deferred.t

  val bury : conn -> id:int -> priority:int -> unit Deferred.t
  val delete : conn -> id:int -> unit Deferred.t
  val touch : conn -> id:int -> unit Deferred.t

  val release : conn -> id:int -> priority:int -> delay:int -> unit Deferred.t
  (** peeks *)
  val peek : conn -> id:int -> Job.t Deferred.t
  val peek_ready : conn -> Job.t Deferred.t
  val peek_delayed : conn -> Job.t Deferred.t
  val peek_buried : conn -> Job.t Deferred.t

  (** kicks *)
  val kick_bound : conn -> bound:int -> [ `Kicked of int ] Deferred.t
  val kick_job : conn -> id:int -> unit Deferred.t
  val stats : conn -> id:int -> (string * string) list Deferred.t
end
