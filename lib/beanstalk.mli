(** Async compatible client to the beanstalkd work queue *)

open Core.Std
open Async.Std (* only dependency is Deferred.t *)

type conn
(** Beanstalkd connection *)

val connect : host:string -> port:int -> conn Deferred.t
(** [connect ~host ~port] returns a Deferred that is resolved when the
    connection * is opened*)

val default_connection : ?host:string -> ?port:int -> unit -> conn Deferred.t
(** [default_connection ()] creates a connection with the default host and
 * port. Port 11300 and localhost*)

val quit : conn -> unit Deferred.t
(** [quit conn] Returns a deferred that is resolved when a connection is closed
 *)

(** Representation of beanstalkd jobs *)
module Job : sig
  type t = private {
    id : int;
    data : string;
  } with sexp
  (** a representation of a beanstalkd job *)
  val id : t -> int
  (** [id j] return the id of job [j]*)
  val data : t -> string
  (* [data j return the raw payload of job [j] *)
end

(** A module to manipulat beanstalkd tubes *)
module Tube : sig
  (** common operations *)
  val all : conn -> string list Deferred.t
  (** [all conn] returns the names of all tubes available *)
  val stats : conn -> tube:string -> (string * string) list Deferred.t
  (** [stats conn ~tube] return a list of statistics for [tube] *)
  val pause : conn -> tube:string -> delay:int -> unit Deferred.t
  (** consumers *)
  val watched : conn -> string list Deferred.t
  (** [watched conn] returns a list of all watched tubes *)
  val watch : conn -> tube:string -> [ `Watching of int ] Deferred.t
  (** [watch conn ~tube] Adds [tube] to the watch list and returns
     the number of watched tubes *)
  val ignore_tube : conn -> tube:string -> unit Deferred.t
  (** [ignore_tube conn ~tube] remove [tube] from the watch list *)
  (** producers *)
  val use : conn -> tube:string -> unit Deferred.t
  val using : conn -> [ `Tube of string ] Deferred.t
end

(** Job operations within beanstalkd *)
module Worker : sig
  val reserve : conn -> Job.t Deferred.t

  val reserve' : conn -> [`Ok of Job.t | `Unavailable ] Deferred.t
  (** [reserve' conn] is like reserve except that it will not block
     the connection to the beanstalkd instance and will resolve immediately if
       a job isn't available *)

  val reserve_timeout : 
    conn -> Time.Span.t -> [`Ok of Job.t | `Timeout ] Deferred.t
  (** [reserve_timeout conn span] will attempt to reserve a job for upto span
     seconds. If after [span] a job isn't found then the deffered resolves to
     `Timeout. Warning: [span] rounds down subsecond percision *)

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
