open Core.Std
open Async.Std

type conn
type conf = (string, string) List.Assoc.t

val connect : ?port:int -> host:string -> conn Deferred.t

module Tube : sig
  open Deferred (* for the correct result type *)
  (** common operations *)
  val all : conn -> string list Or_error.t
  val stats : conn -> tube:string  -> conf Or_error.t
  val pause : conn -> tube:string -> unit Or_error.t
  (** consumers *)
  val watched : conn -> string list Or_error.t
  val watch : conn -> tube:string -> [`Watching of int] Or_error.t
  val ignore : conn -> tube:string -> unit Or_error.t
  (** producers *)
  val use : conn -> tube:string -> unit Or_error.t
  val used : conn -> string Or_error.t
end

module type Serializable = sig
  type t
  val serialize : t -> string
  val deserialize : string -> t
  val size : t -> int
end

module Job : sig
  type t
  include Serializable with type t := t
  val id : t -> int
  val data : t -> string
end

module Worker : sig
  open Deferred (* for the correct result type *)
  type t = Job.t
  (** reserving jobs *)
  val reserve : conn -> ?timeout:int -> t Or_error.t

  (** job operations *)
  val put : conn -> ?delay:int -> priority:int -> ttr:int -> unit Or_error.t
  val bury : conn -> id:int -> priority:int -> unit Or_error.t
  val delete : conn -> id:int -> unit Or_error.t
  val touch : conn -> id:int -> unit Or_error.t
  val release : conn -> id:int -> priority:int -> unit Or_error.t

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

