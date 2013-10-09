open Core.Std
type error =
  | Timed_out
  | Out_of_memory
  | Draining
  | Unknown_command
  | Buried of int option
  | Job_too_big
  | Deadline_soon
  | Not_ignored
  | Not_connected
  | Invalid_tube_name
  | Job_not_reserved
  | Beanstalk_not_found
  | Expected_crlf
  | Bad_format
  | Internal_error
  | Unexpected_response of string
  with sexp

exception Beanstalk_error of error with sexp

let raise_b e = raise (Beanstalk_error e) 
