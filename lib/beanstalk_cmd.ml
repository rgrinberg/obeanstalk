open Core.Std
open Async.Std

let split s ~on = 
  let open String in
  let i = index_exn s on in
  let len = (length s) - i in
  (sub s ~pos:0 ~len:i, sub s ~pos:(i+1) ~len:(len-1))

(* just enough to parse whatever we get from obeanstalk *)
(* TODO : this function is still untested *)
let parse_yaml_dict s = 
  match String.split s ~on:'\n' with
  | [] -> [] (* first element should be header *)
  | _::lines -> (* I assume first line is the header *)
    lines |> List.filter_map ~f:(fun l -> 
      try let (k,v) = split ~on:':' l in Some (k, String.strip v)
      (* TODO : fix this "inelegant" error handling *)
      with _ -> (Printf.printf "Could not parse '%s'\n" l; None))

let wrap x = x ^ "\r\n"

let unwrap x = 
  let len = String.length x in
  assert (x.[len-1] = '\n' && x.[len-2] = '\r');
  String.sub ~pos:0 ~len:(len-2) x

(* strip \r\n from the end of the line *)
let unwrap_smart x = 
  let count = ref 0 in
  let len = x |> String.length in
  (try if x.[len-1] = '\n' then incr count with _ -> ());
  (try if x.[len-2] = '\r'then incr count with _ -> ());
  String.sub ~pos:0 ~len:(len - (!count)) x

module Command = struct
  let sp = Printf.sprintf
  let use_tube ~tube = (sp "use %s" tube) |> wrap
  let put ?(delay=0) ~priority ~ttr ~bytes =
    (sp "put %d %d %d %d" priority delay ttr bytes) |> wrap
  let reserve = wrap "reserve"
  let reserve_timeout ~timeout = (sp "reserve-with-timeout %d" timeout) |> wrap
  let delete ~id = (sp "delete %d" id) |> wrap
  let release ~id ~priority ~delay =
    (sp "release %d %d %d" id priority delay) |> wrap
  let bury ~id ~priority = (sp "bury %d %d" id priority) |> wrap
  let touch ~id = (sp "touch %d" id) |> wrap
  let watch ~tube = (sp "watch %s" tube) |> wrap
  let ignore_tube ~tube = (sp "ignore %s" tube) |> wrap
  let peek ~id = (sp "peek %d" id) |> wrap
  let peek_ready = "peek-ready" |> wrap
  let peek_delayed = "peek-delayed" |> wrap
  let peek_buried = "peek-buried" |> wrap
  let kick ~bound = (sp "kick %d" bound) |> wrap
  let kick_job ~id = (sp "kick-job %d" id) |> wrap
  let stats_job ~id = (sp "stats-job %d" id) |> wrap (* returns YAML *)
  let stats_tube ~name = (sp "stats-tube %s" name) |> wrap
  let stats = "stats" |> wrap
  let list_tubes = "list-tubes" |> wrap
  let list_tube_used = "list-tube-used" |> wrap
  let list_tubes_watched = "list-tubes-watched" |> wrap
  let quit = "quit" |> wrap
  let pause_tube ~tube ~delay = (sp "pause-tube %s %d" tube delay) |> wrap
end

module Response = struct
  (* functions in this module either return the parsed response or throw
   * a Parse_failed exception. This should probably be changed to use
   * option types. *)
  exception Parse_failed
  type with_id = string -> [`Id of int]

  let group x = "\\(" ^ x ^ "\\)"

  let single_parse ~prefix ~re ~success_protect= 
    let rex = Str.regexp ("^" ^ prefix ^ (group re) ^ "$") in
    (fun s -> 
      try ignore (Str.string_match rex s 0);
        (Str.matched_group 1 s) |> success_protect
      with _ -> raise Parse_failed)

  let tuple_parse ~prefix ~first ~second = 
    let groups = (first |> fst |> group) ^ " " ^ (second |> fst |> group) in
    let open Str in 
    let rex = regexp ("^" ^ prefix ^ " " ^ groups) in
    (fun s ->
      try
        ignore (string_match rex s 0);
        ((matched_group 1 s) |> (snd first),
         (matched_group 2 s) |> (snd second))
      with _ -> raise Parse_failed)

  let job_parse ~prefix = 
    single_parse ~prefix ~re:"[0-9]+"
      ~success_protect:(fun s -> `Id(Int.of_string s))

  let put : with_id = job_parse ~prefix:"INSERTED"
  let bury : with_id = job_parse ~prefix:"BURIED"
  let delete : with_id = job_parse ~prefix:"DELETED"

  let using = single_parse ~prefix:"USING" ~re:".+"
      ~success_protect:(fun s -> `Tube s)

  let fail_if_unequal eq s = if s = eq then `Ok else raise Parse_failed

  let release = fail_if_unequal "RELEASED"
  let touch = fail_if_unequal "TOUCHED"
  let kick_job = fail_if_unequal "KICKED"

  let watch = single_parse ~prefix:"WATCHING" ~re:"\\d+"
      ~success_protect:(fun s -> `Watching (Int.of_string s))

  let ignore_tube = watch (* same response in both cases *)

  let peek_any = tuple_parse ~prefix:"FOUND"
      ~first:("\\d+", (fun s -> `Id(Int.of_string s)))
      ~second:("\\d+",  (fun s -> `Bytes(Int.of_string s)))

  let kick = single_parse ~prefix:"KICKED" ~re:"\\d+"
      ~success_protect:(fun s -> `Kicked (Int.of_string s))

  let stats_tube = single_parse ~prefix:"OK" ~re:"\\d+"
      ~success_protect:(fun s -> `Bytes (Int.of_string s)) (* YAML *)

  let reserve s = (`Id (failwith "TODO"), `Bytes (failwith "TODO"))

  let list_tubes_any = stats_tube

  let pause_tube = fail_if_unequal "PAUSED"

  let try_with t ~resp = Or_error.try_with (fun () -> (t resp))
  let try_with_ignore t ~resp = Or_error.try_with (fun () -> ignore (t resp))
end
