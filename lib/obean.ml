open Core.Std
open Async.Std
open Async_unix.Async_print

let readme () = "Example:
obean blah blah"

let conn () = Obeanstalk.connect ~port:11300 ~host:"127.0.0.1"

let print_tubes conn =
  conn >>= (fun bs -> Obeanstalk.Tube.all bs)
  >>= begin fun res ->
    begin match res with
      | Result.Ok s -> begin
          print_endline "tubes:";
          List.iter s ~f:print_endline
        end
      | Result.Error _ -> print_endline "uh oh" end;
    return ()
  end

let print_stats conn ~tube =
  conn >>= (fun bs -> Obeanstalk.Tube.stats bs ~tube) >>= begin fun stats->
    (match stats with
    | Result.Ok s -> 
        s |> List.iter ~f:(fun (key, value) -> printf "%s = %s\n" key value)
    | Result.Error _ -> print_endline "uh oh");
    return ()
  end

let tube =
  Command.async_basic ~summary:"Inspect tubes"
    Command.Spec.(
      empty
      +> flag "-s" (optional_with_default "default" string)
          ~doc:"stats of a tube"
      +> flag "-l" (no_arg)
          ~doc:"list tubes"
    ) (fun tube list_tubes () ->
      if list_tubes then () |> conn |> print_tubes
      else print_stats (conn ()) ~tube)

let command =
  Command.group ~summary:"beanstalkd command line tool"
  ["tube", tube]

let () = Exn.handle_uncaught ~exit:true (fun () -> Command.run command)
