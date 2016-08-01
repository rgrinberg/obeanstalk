open Core.Std

exception Timed_out [@@deriving sexp]
exception Expected_crlf [@@deriving sexp]
exception Bad_format [@@deriving sexp]
exception Internal_error [@@deriving sexp]
exception Unexpected_response of string [@@deriving sexp]
exception Unknown_command [@@deriving sexp]


type error =
  | Out_of_memory
  | Draining
  | Buried of int option
  | Job_too_big
  | Deadline_soon
  | Not_ignored 
  | Beanstalk_not_found
  [@@deriving sexp]

exception Beanstalk_error of error [@@deriving sexp]

let raise_b e = raise (Beanstalk_error e) 
