open Core.Std

type error =
  | Unexpected_response of string
  | Timed_out
  | Out_of_memory
  | Internal_error
  | Draining
  | Bad_format
  | Unknown_command
  | Buried of int option
  | Expected_crlf
  | Job_too_big
  | Deadline_soon
  | Not_ignored
  | Not_connected
  | Invalid_tube_name
  | Job_not_reserved
  | Beanstalk_not_found with sexp

exception Beanstalk_error of error with sexp

let raise_b e = raise (Beanstalk_error e) 
