(** Async compatible client to the beanstalkd work queue *)

open Core.Std
open Async.Std

type conn

val connect : port:int -> host:string -> conn Deferred.t

val default_connection : ?port:int -> ?host:string -> unit -> conn Deferred.t

val quit : conn -> unit Deferred.t

module type Serializable = sig
  type t
  val serialize : t -> string
  val deserialize : string -> t
  val size : t -> int
end

module Job : sig
  type 'a t
  val id : 'a t -> int
  val data : 'a t -> 'a
  val create : data: 'a -> id:int -> 'a t
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

module Worker : functor (S : Serializable) -> sig
  type s = S.t
  type t = s Job.t

  val reserve : ?timeout:int -> conn -> t Deferred.t
  (** job operations *)
  val put : conn -> ?delay:int ->
    priority:int -> ttr:int -> job:s -> t Deferred.t

  val bury : conn -> id:int -> priority:int -> unit Deferred.t
  val delete : conn -> id:int -> unit Deferred.t
  val touch : conn -> id:int -> unit Deferred.t

  val release : conn -> id:int -> priority:int -> delay:int -> unit Deferred.t
  (** peeks *)
  val peek : conn -> id:int -> t Deferred.t
  val peek_ready : conn -> t Deferred.t
  val peek_delayed : conn -> t Deferred.t
  val peek_buried : conn -> t Deferred.t

  (** kicks *)
  val kick_bound : conn -> bound:int -> [ `Kicked of int ] Deferred.t
  val kick_job : conn -> id:int -> unit Deferred.t
  val stats : conn -> id:int -> (string * string) list Deferred.t
end

module Stringly : sig
  module Worker : sig
    type t = string Job.t

    val reserve : ?timeout:int -> conn -> t Deferred.t

    val put : conn -> ?delay:int ->
      priority:int -> ttr:int -> job:string -> t Deferred.t

    val bury : conn -> id:int -> priority:int -> unit Deferred.t

    val delete : conn -> id:int -> unit Deferred.t
    val touch : conn -> id:int -> unit Deferred.t

    val release : conn ->
      id:int -> priority:int -> delay:int -> unit Deferred.t

    val peek : conn -> id:int -> t Deferred.t
    val peek_ready : conn -> t Deferred.t
    val peek_delayed : conn -> t Deferred.t
    val peek_buried : conn -> t Deferred.t

    val kick_bound : conn -> bound:int -> [ `Kicked of int ] Deferred.t

    val kick_job : conn -> id:int -> unit Deferred.t

    val stats : conn -> id:int -> (string * string) list Deferred.t
  end
end
