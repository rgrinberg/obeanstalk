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

module Tube : sig
  val all : conn -> string list Deferred.t
  val stats : conn -> tube:string  -> (string * string) list Deferred.t
  val pause : conn -> tube:string -> delay:int -> unit Deferred.t
  val watched : conn -> string list Deferred.t
  val watch : conn -> tube:string -> [`Watching of int] Deferred.t
  val ignore_tube : conn -> tube:string -> unit Deferred.t
  val use : conn -> tube:string -> unit Deferred.t
  val using : conn -> [`Tube of string] Deferred.t
end

module Job : sig
  type 'a t
  val id : 'a t -> int
  val data : 'a t -> 'a
end

module Worker : functor (S : Serializable) -> sig
  type job = S.t Job.t
  (** reserving jobs *)
  val reserve : ?timeout:int -> conn -> job Deferred.t
  (** job operations *)
  val put : conn -> ?delay:int -> priority:int -> ttr:int -> 
                    data:S.t -> job Deferred.t

  val bury : conn -> id:int -> priority:int -> unit Deferred.t
  val delete : conn -> id:int -> unit Deferred.t
  val touch : conn -> id:int -> unit Deferred.t
  val release : conn -> id:int -> priority:int -> delay:int ->  unit Deferred.t
  (** peeks *)
  val peek : conn -> id:int -> job Deferred.t
  val peek_ready : conn -> job Deferred.t
  val peek_delayed : conn -> job Deferred.t
  val peek_buried : conn -> job Deferred.t
  (** kicks *)
  val kick_bound : conn -> bound:int -> [ `Kicked of int ] Deferred.t
  val kick_job : conn -> id:int -> unit Deferred.t

  val stats : conn -> id:int -> (string * string) list Deferred.t
end

