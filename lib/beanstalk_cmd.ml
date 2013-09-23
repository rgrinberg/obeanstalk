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

  let create_ints ~name ~args =
    {name; args=(args |> List.map ~f:Int.to_string)}

  let to_string {name; args} = String.concat ~sep:" " (name::args)

  let of_string s = 
    match String.split ~on:' ' s with
    | [] -> invalid_arg ("Commnad.of_string: " ^ s)
    | name::args -> {name; args}

  let size {args;_} = args |> List.last_exn |> Int.of_string
  (* common constructors *)
  let no_args name     = {name; args=[]}
  let one_arg name arg = {name; args=[arg]}
  let one_id name id   = one_arg name (Int.to_string id)
end

module Request = struct
  type t =
    | Single of Command.t
    | WithJob of Command.t * string

  let use_tube ~tube = Single(Command.one_arg "use" tube)

  let put ?(delay=0) ~priority ~ttr ~bytes ~job =
    WithJob(Command.create_ints ~name:"put" 
        ~args:[priority;delay;ttr;bytes], job)

  let reserve = Single(Command.no_args "reserve")

  let reserve_timeout ~timeout = 
    Single(Command.one_arg "reserve-with-timeout" (Int.to_string timeout))

  let delete ~id = Single(Command.one_arg "delete" (Int.to_string id))

  let release ~id ~priority ~delay =
    Single(Command.create_ints ~name:"release" ~args:[id;priority;delay])

  let bury ~id ~priority = 
    Single(Command.create ~name:"bury"
        ~args:[Int.to_string id; Int.to_string priority])

  let touch ~id = Single (Command.one_arg "touch" (Int.to_string id))

  let watch ~tube       = Single(Command.one_arg "watch" tube)
  let ignore_tube ~tube = Single(Command.one_arg "ignore" tube)

  let peek ~id     = Single(Command.one_id "peek" id)
  let peek_ready   = Single(Command.no_args "peek-ready")
  let peek_delayed = Single(Command.no_args "peek-delayed")
  let peek_buried  = Single(Command.no_args "peek-buried")

  let kick_bound ~bound = Single(Command.one_id "kick" bound)

  let kick_job ~id       = Single(Command.one_id "kick-job" id)
  let stats_job ~id      = Single(Command.one_id "stats-job" id)
  let stats_tube ~tube   = Single(Command.one_arg "stats-tube" tube)
  let stats              = "stats"
  let list_tubes         = Single(Command.no_args "list-tubes")
  let list_tube_used     = Single(Command.no_args "list-tube-used")
  let list_tubes_watched = Single(Command.no_args "list-tubes-watched")
  let quit               = "quit"

  let pause_tube ~tube ~delay =
    Single(Command.create ~name:"pause-tube" 
        ~args:[tube;(Int.to_string delay)])
end

module Response = struct
  (* functions in this module either return the parsed response or throw
   * a Parse_failed exception. This should probably be changed to use
   * option types. For now we will ignore any errors caused by parse
   * failure. errors are handled much earlier anyway. *)
  exception Parse_failed

  let verify ~is s = if s <> is then raise Parse_failed

  let verify_only ~is = `Single(fun {Command.name;_} -> verify name ~is)

  let put = `Single(fun {Command.name;args} ->
      verify name ~is:"INSERTED";
      (`Id (args |> List.hd_exn |> Int.of_string)))

  let bury = verify_only ~is:"BURIED"

  let delete = verify_only ~is:"DELETED"

  let using = `Single(fun {Command.name; args} ->
      verify name ~is:"USING"; `Tube (List.hd_exn args))

  let fail_if_unequal eq s = if s = eq then `Ok else raise Parse_failed

  let release = verify_only ~is:"RELEASED"

  let touch = verify_only ~is:"TOUCHED"

  let kick_job = verify_only ~is:"KICKED"

  let watch = `Single(fun {Command.name;args} ->
      verify name ~is:"WATCHING";
      `Watching (args |> List.hd_exn |> Int.of_string))

  let ignore_tube = watch

  let peek_any = `WithPayload (fun {Command.name; args} ->
      verify name ~is:"FOUND";
      let id = args |> List.hd_exn |> Int.of_string in 
      fun x -> (`Id id, Payload.Job(x)))

  let kick_bound = `Single(fun {Command.name;args} ->
      verify name ~is:"KICKED"; `Kicked (args |> List.hd_exn |> Int.of_string))

  let stats_job = `WithPayload (fun {Command.name; _} ->
      verify name ~is:"OK"; (fun x -> Payload.YDict x))

  let stats_tube = stats_job

  let reserve = `WithPayload (fun {Command.name; args} ->
      verify name ~is:"RESERVED";
      let id = args |> List.hd_exn |> Int.of_string in 
      fun x -> (`Id id, Payload.Job(x)))

  let list_tubes_any = `WithPayload (fun {Command.name ;_} ->
      verify name ~is:"OK"; (fun x -> Payload.YList x))

  let pause_tube = `Single (fun {Command.name; _} ->
      verify name ~is:"PAUSED")

  let try_with t ~resp = Or_error.try_with (fun () -> (t resp))
  let try_with_ignore t ~resp = Or_error.try_with (fun () -> ignore (t resp))
end
