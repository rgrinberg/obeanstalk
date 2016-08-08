(** Async compatible client to the beanstalk work queue. Constult the
    beanstalk protocol docs for more thorough coverage of the protocol *)

open Core.Std
open Async.Std (* only dependency is Deferred.t *)

type conn
(** beanstalk connection *)

val connect : host:string -> port:int -> conn Deferred.t
(** [connect ~host ~port] returns a Deferred that is resolved when the
    connection is opened*)

val default_connection : ?host:string -> ?port:int -> unit -> conn Deferred.t
(** [default_connection ()] creates a connection with the default host and
    port. Port 11300 and localhost*)

val quit : conn -> unit Deferred.t
(** [quit conn] Returns a deferred that is resolved when a connection is closed
*)

(** Representation of beanstalk jobs *)
module Job : sig
  type t = private {
    id : int;
    data : string;
  } [@@deriving sexp]
  (** a representation of a beanstalk job *)
  val id : t -> int
  (** [id j] return the id of job [j]*)
  val data : t -> string
  (* [data j return the raw payload of job [j] *)
end

(** A module to manipulate beanstalk tubes *)
module Tube : sig
  val all : conn -> string list Deferred.t
  (** [all conn] returns the names of all tubes available *)
  val stats : conn -> tube:string -> (string * string) list Deferred.t
  (** [stats conn ~tube] return a list of statistics for [tube] *)
  val pause : conn -> tube:string -> delay:int -> unit Deferred.t
  (** [pause conn ~tube ~delay] Set a delay of [delay] seconds on [tube].
      Hence any job reserves from this tube will be delayed by at least
      [delay] seconds*)
  val watched : conn -> string list Deferred.t
  (** [watched conn] returns a list of all watched tubes *)
  val watch : conn -> tube:string -> [ `Watching of int ] Deferred.t
  (** [watch conn ~tube] Adds [tube] to the watch list and returns
      the number of watched tubes *)
  val ignore_tube : conn -> tube:string -> unit Deferred.t
  (** [ignore_tube conn ~tube] remove [tube] from the watch list *)
  val use : conn -> tube:string -> unit Deferred.t
  (** [use conn ~tube] Subsequent [Worker.put] commands will put jobs
      into [tube] *)
  val using : conn -> [ `Tube of string ] Deferred.t
  (** [using conn] Returns the currently used tube *)
end

(** Job operations within beanstalk *)
module Worker : sig
  val reserve : conn -> Job.t Deferred.t
  (** [reserve conn] Determined when a job is leased from [conn].
      [Beanstalk_error(Deadline_soon)] can be thrown is too many reserve
      calls are made *)

  val reserve_now : conn -> [`Ok of Job.t | `Timed_out ] Deferred.t
  (** [reserve_now conn] is like reserve except that it will not block the
      connection to the beanstalk instance and will resolve immediately if
      a job isn't available *)

  val reserve_timeout : 
    conn -> Time.Span.t -> [`Ok of Job.t | `Timed_out ] Deferred.t
  (** [reserve_timeout conn span] will attempt to reserve a job for upto
      span seconds. If after [span] a job isn't found then the deffered
      resolves to `Timeout. Warning: [span] rounds down subsecond percision *)

  val put : ?delay:int -> ?priority:int -> ?ttr:int
    -> conn -> data:string -> Job.t Deferred.t
  (** [put ?delay ?priority ?ttr conn ~data] Creates a job with payload
      [data]. Smaller [priority] jobs will be scheduled before jobs with
      larger priorities. [ttr] is the maximum time in seconds a worker
      is allowed to reserve a job without deleting/releasing/burying it.
      Default is 1 second. [delay] is the number of seconds to wait before
      putting the job in the read queue. Default is 0*)

  val bury : conn -> id:int -> priority:int -> unit Deferred.t
  (** [bury conn ~id ~priority] buries job [id] and assigns it a new
      [priority]*)
  val delete : conn -> id:int -> unit Deferred.t
  (** [delete conn ~id] deletes job [id] *)
  val touch : conn -> id:int -> unit Deferred.t
  (** [touch conn ~id] "touches" a job [id] I.e. asks the server
      for more time to process it before it starts replying with Deadline_soon*)

  val release : conn -> id:int -> priority:int -> delay:int -> unit Deferred.t
  (** [release conn ~id ~priority ~delay] Puts the reserved job [id] back
      in the ready queue with a [priority] and [delay]*)

  val peek : conn -> id:int -> Job.t Deferred.t
  (** [peek conn ~id] Return the job [id] *)
  val peek_ready : conn -> Job.t Deferred.t
  (** [peek_ready conn] Return the next job in the ready queue*)
  val peek_delayed : conn -> Job.t Deferred.t
  (** [peek_delayed conn] Return the next job in the delayed queue *)
  val peek_buried : conn -> Job.t Deferred.t
  (** [peek_buried conn] Return the next job in the buried queue *)

  val kick_bound : conn -> bound:int -> [ `Kicked of int ] Deferred.t
  (** [kick_bound conn ~bound] Kicks upto [bound] jobs into the ready
      queue. Returns the number of jobs kicked. If there are no buried jobs 
      to kick then it will kick delayed jobs*)
  val kick : conn -> id:int -> unit Deferred.t
  (** [kick conn ~id] Kicks the the job [id] *)
  val stats : conn -> id:int -> (string * string) list Deferred.t
  (** [stats conn ~id] Returns statistics about job [id] *)
end

type error =
  | Out_of_memory
  (** Raised when beanstalk cannot allocate enough memory for the job*)
  | Draining
  (** Raised when the server is no longer accepting new jobs *)
  | Buried of int option
  (** Raised when the server runs out of memory releasing/putting a job
      An id is provided on put to identify the job*)
  | Job_too_big
  (** Client has requested to put a job with a body larger than max-job-size bytes *)
  | Deadline_soon
  (** Client has reserved too many jobs without releasing/deleting them
      and is not allowed to reserve more jobs.*)
  | Not_ignored 
  (** Client attempts to ignore only tube in its watch list *)
  | Beanstalk_not_found
  (** Requested job/tube is not found *)
[@@deriving sexp]

exception Beanstalk_error of error [@@deriving sexp]
