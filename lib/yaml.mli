(** YAML parsing functions for parsing eanstalkd responses *)

(** parse a dictionary. assume all values are strings *)
val to_dict : string -> (string * string) list

(** parse a yaml list of strings *)
val to_list : string -> string list

(** ONLY FOR TESTS *)
val split_exn : string -> on:char -> string * string

