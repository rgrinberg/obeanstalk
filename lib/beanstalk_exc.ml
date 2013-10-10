open Core.Std

exception Timed_out with sexp
exception Expected_crlf with sexp
exception Bad_format with sexp
exception Internal_error with sexp
exception Unexpected_response of string with sexp
exception Unknown_command with sexp


type error =
  | Out_of_memory
  | Draining
  | Buried of int option
  | Job_too_big
  | Deadline_soon
  | Not_ignored 
  | Beanstalk_not_found
  with sexp

exception Beanstalk_error of error with sexp

let raise_b e = raise (Beanstalk_error e) 
