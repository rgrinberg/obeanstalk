open Core.Std

exception Unexpected_response of string with sexp
exception Timeout with sexp
exception Out_of_memory with sexp
exception Internal_error with sexp
exception Draining with sexp
exception Bad_format with sexp
exception Unknown_command with sexp
exception Buried of (int option) with sexp
exception Expected_crlf with sexp
exception Job_too_big with sexp
exception Deadline_soon with sexp
exception Not_ignored with sexp
exception Not_connected with sexp
exception Invalid_tube_name with sexp
exception Job_not_reserved with sexp
exception Beanstalk_not_found with sexp

let raise_if_error s = 
  match (String.split s ~on:' ') with
  | "NOT_FOUND"::_       -> raise Beanstalk_not_found
  | "TIMED_OUT"::_       -> raise Timeout
  | "OUT_OF_MEMORY"::_   -> raise Out_of_memory
  | "INTERNAL_ERROR"::_  -> raise Internal_error
  | "DRAINING"::_        -> raise Draining
  | "BAD_FORMAT"::_      -> raise Bad_format
  | "UNKNOWN_COMMAND"::_ -> raise Unknown_command
  | "EXPECTED_CRLF"::_   -> raise Expected_crlf
  | "JOB_TOO_BIG"::_     -> raise Job_too_big
  | "DEADLINE_SOON"::_   -> raise Deadline_soon
  | "NOT_IGNORED"::_     -> raise Not_ignored
  | "BURIED"::[id_s]     -> raise (Buried (Some (int_of_string id_s)))
  | "BURIED"::[]         -> raise (Buried None)
  | _                    -> ()
