exception Unexpected_response of string with sexp
exception Timeout                       with sexp
exception Out_of_memory                 with sexp
exception Internal_error                with sexp
exception Draining                      with sexp
exception Bad_format                    with sexp
exception Unknown_command               with sexp
exception Buried of int option          with sexp
exception Expected_crlf                 with sexp
exception Job_too_big                   with sexp
exception Deadline_soon                 with sexp
exception Not_ignored                   with sexp
exception Not_connected                 with sexp
exception Invalid_tube_name             with sexp
exception Job_not_reserved              with sexp
exception Beanstalk_not_found           with sexp
