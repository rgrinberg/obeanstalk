(** YAML parsing functions for parsing beanstalkd responses *)

(** parse a dictionary. assume all values are strings *)
val to_dict : string -> (string * string) list

(** parse a yaml list of strings *)
val to_list : string -> string list
