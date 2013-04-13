open Core.Std
open Async.Std

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

module Payload = struct
  type _ t = 
    | YList : string -> (string list) t
    | YDict : string -> ((string * string) list) t
    | Job : string -> string t
end

module Command = struct
  type t = {
    name : string;
    args : string list; 
  } with sexp

  type 'a reader = t -> 'a

  let create ~name ~args = {name;args}

  let to_string {name; args} = String.concat ~sep:" " (name::args)
  let of_string s = 
    match String.split ~on:' ' s with
    | [] -> assert false
    | name::args -> {name; args}

  let size {args;_} = args |> List.last_exn |> Int.of_string
  (* common constructors *)
  let no_args name = {name; args=[]}
  let one_arg name arg = {name; args=[arg]}
end

module Request = struct
  type t =
    | Single of Command.t
    | WithJob of Command.t * string

  let sp = Printf.sprintf

  let use_tube ~tube = Single(Command.one_arg "use" tube)

  let put ?(delay=0) ~priority ~ttr ~bytes =
    (sp "put %d %d %d %d" priority delay ttr bytes)
  let reserve = wrap "reserve"
  let reserve_timeout ~timeout = (sp "reserve-with-timeout %d" timeout)
  let delete ~id = (sp "delete %d" id)
  let release ~id ~priority ~delay =
    (sp "release %d %d %d" id priority delay)
  let bury ~id ~priority = (sp "bury %d %d" id priority)
  let touch ~id = (sp "touch %d" id)
  let watch ~tube = Single(Command.one_arg "watch" tube)
  let ignore_tube ~tube = Single(Command.one_arg "ignore" tube)
  let peek ~id = (sp "peek %d" id)
  let peek_ready = "peek-ready"
  let peek_delayed = "peek-delayed"
  let peek_buried = "peek-buried"
  let kick ~bound = (sp "kick %d" bound)
  let kick_job ~id = (sp "kick-job %d" id)
  let stats_job ~id = (sp "stats-job %d" id) (* returns YAML *)
  let stats_tube ~tube = Single(Command.one_arg "stats-tube" tube)
  let stats = "stats"
  let list_tubes = Single(Command.no_args "list-tubes")
  let list_tube_used = Single(Command.no_args "list-tube-used")
  let list_tubes_watched = Single(Command.no_args "list-tubes-watched")
  let quit = "quit"

  let pause_tube ~tube ~delay =
    Single(Command.create ~name:"pause-tube" 
                          ~args:[tube;(Int.to_string delay)])
end

module Response = struct
  (* this type isn't referenced anywhere it's just for documentation 
   * for now *)
  type 'result t = [
    | `Single of 'result Command.reader
    | `WithPayload of (string -> 'result) Command.reader ]

  (* functions in this module either return the parsed response or throw
   * a Parse_failed exception. This should probably be changed to use
   * option types. For now we will ignore any errors caused by parse
   * failure. errors are handled much earlier anyway. *)
  exception Parse_failed
  type with_id = string -> [`Id of int]

  let group x = "\\(" ^ x ^ "\\)"

  let verify ~is s = if s <> is then raise Parse_failed

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

  let using = `Single(fun {Command.name; args} ->
    verify name ~is:"USING"; `Tube (List.hd_exn args))

  let fail_if_unequal eq s = if s = eq then `Ok else raise Parse_failed

  let release = fail_if_unequal "RELEASED"
  let touch = fail_if_unequal "TOUCHED"
  let kick_job = fail_if_unequal "KICKED"

  let watch = `Single(fun {Command.name;args} ->
    verify name ~is:"WATCHING";
    `Watching (args |> List.hd_exn |> Int.of_string))

  let ignore_tube = watch
  
  let peek_any = tuple_parse ~prefix:"FOUND"
      ~first:("\\d+", (fun s -> `Id(Int.of_string s)))
      ~second:("\\d+",  (fun s -> `Bytes(Int.of_string s)))

  let kick = single_parse ~prefix:"KICKED" ~re:"\\d+"
      ~success_protect:(fun s -> `Kicked (Int.of_string s))

  let stats_job = single_parse ~prefix:"OK" ~re:"\\d+"
      ~success_protect:(fun s -> `Bytes (Int.of_string s)) (* YAML *)

  let stats_tube = `WithPayload (fun {Command.name; _} ->
    verify name ~is:"OK"; (fun x -> Payload.YDict(x)))

  let reserve s = (`Id (failwith "TODO"), `Bytes (failwith "TODO"))

  let list_tubes_any = `WithPayload (fun {Command.name ;_} ->
      verify name ~is:"OK"; (fun x -> Payload.YList(x)))

  let pause_tube = `Single (fun {Command.name; _} ->
    verify name ~is:"PAUSED")

  let try_with t ~resp = Or_error.try_with (fun () -> (t resp))
  let try_with_ignore t ~resp = Or_error.try_with (fun () -> ignore (t resp))
end
