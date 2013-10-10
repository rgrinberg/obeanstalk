
val wrap         : string -> string
val unwrap       : string -> string
val raise_if_error : string -> unit

module Payload : sig
  type _ t =
      YList : string -> string list t
    | YDict : string -> (string * string) list t
    | Job : string -> string t
end
module Command : sig
  type t = { 
    name : string; 
    args : string list; 
  } with sexp

  val create      : name:string -> args:string list -> t
  val create_ints : name:string -> args:int list -> t
  val to_string   : t -> string
  val of_string   : string -> t
  val size        : t -> int
  val no_args     : string -> t
  val one_arg     : string -> string -> t
  val one_id      : string -> int -> t
end
module Request : sig
  type t = 
    | Single of Command.t 
    | WithJob of Command.t * string

  val use_tube : tube:string -> t
  val put :
    ?delay:int -> ?priority:int -> ?ttr:int -> bytes:int -> job:string -> t
  val reserve            : t
  val reserve_timeout    : timeout:int -> t
  val delete             : id:int -> t
  val release            : id:int -> priority:int -> delay:int -> t
  val bury               : id:int -> priority:int -> t
  val touch              : id:int -> t
  val watch              : tube:string -> t
  val ignore_tube        : tube:string -> t
  val peek               : id:int -> t
  val peek_ready         : t
  val peek_delayed       : t
  val peek_buried        : t
  val kick_bound         : bound:int -> t
  val kick_job           : id:int -> t
  val stats_job          : id:int -> t
  val stats_tube         : tube:string -> t
  val stats              : string
  val list_tubes         : t
  val list_tube_used     : t
  val list_tubes_watched : t
  val quit               : string
  val pause_tube         : tube:string -> delay:int -> t
end
module Response : sig
  exception Parse_failed
  val verify          : is:'a -> 'a -> unit
  val verify_only     : is:string -> [> `Single of Command.t -> unit ]
  val put             : [> `Single of Command.t -> [> `Id of int ] ]
  val bury            : [> `Single of Command.t -> unit ]
  val delete          : [> `Single of Command.t -> unit ]
  val using           : [> `Single of Command.t -> [> `Tube of string ] ]
  val fail_if_unequal : 'a -> 'a -> [> `Ok ]
  val release         : [> `Single of Command.t -> unit ]
  val touch           : [> `Single of Command.t -> unit ]
  val kick_job        : [> `Single of Command.t -> unit ]
  val watch           : [> `Single of Command.t -> [> `Watching of int ] ]
  val ignore_tube     : [> `Single of Command.t -> [> `Watching of int ] ]

  val peek_any :
    [> `WithPayload of
         Command.t -> string -> [> `Id of int ] * string Payload.t ]

  val stats_job :
    [> `WithPayload of
         Command.t -> string -> (string * string) list Payload.t ]

  val stats_tube :
    [> `WithPayload of
         Command.t -> string -> (string * string) list Payload.t ]

  val reserve :
    [> `WithPayload of
         Command.t -> string -> [> `Id of int ] * string Payload.t ]

  val list_tubes_any :
    [> `WithPayload of Command.t -> string -> string list Payload.t ]

  val kick_bound      : [> `Single of Command.t -> [> `Kicked of int ] ]
  val pause_tube      : [> `Single of Command.t -> unit ]
end
