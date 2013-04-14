open Core.Std
open Async.Std

type conn
type conf = (string, string) List.Assoc.t

val connect : port:int -> host:string -> conn Deferred.t
val quit : conn -> unit Deferred.t

module type Serializable = sig
  type t
  val serialize : t -> string
  val deserialize : string -> t
  val size : t -> int
end

module Tube : sig
  open Deferred (* for the correct result type *)
  (** common operations *)
  val all : conn -> string list Or_error.t
  val stats : conn -> tube:string  -> conf Or_error.t
  val pause : conn -> tube:string -> delay:int -> unit Or_error.t
  (** consumers *)
  val watched : conn -> string list Or_error.t
  val watch : conn -> tube:string -> [`Watching of int] Or_error.t
  val ignore_tube : conn -> tube:string -> unit Or_error.t
  (** producers *)
  val use : conn -> tube:string -> unit Or_error.t
  val using : conn -> [`Tube of string] Or_error.t
end

module type Job_intf = sig
  type 'a t
  val id : 'a t -> int
  val data : 'a t -> 'a
  val create : data: 'a -> id:int -> 'a t
end

module Worker : functor (S : Serializable) -> sig
  module Job : Job_intf
  open Deferred (* for the correct result type *)
  type s = S.t
  type t = s Job.t
  (** reserving jobs *)
  val reserve : ?timeout:int -> conn -> t Or_error.t
  (** job operations *)

  val put : conn -> ?delay:int -> priority:int -> ttr:int -> 
                    job:S.t -> t Or_error.t

  val bury : conn -> id:int -> priority:int -> unit Or_error.t
  val delete : conn -> id:int -> unit Or_error.t
  val touch : conn -> id:int -> unit Or_error.t
  val release : conn -> id:int -> priority:int -> delay:int ->  unit Or_error.t
  (** peeks *)
  val peek : conn -> id:int -> t Or_error.t
  val peek_ready : conn -> t Or_error.t
  val peek_delayed : conn -> t Or_error.t
  val peek_buried : conn -> t Or_error.t
  (** kicks *)
  val kick_bound : conn -> bound:int -> [ `Kicked of int ] Or_error.t
  val kick_job : conn -> id:int -> unit Or_error.t

  val stats : conn -> id:int -> conf Or_error.t
end

