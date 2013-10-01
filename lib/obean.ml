open Core.Std
open Async.Std
open Async_unix.Async_print

let conn () = 
  Monitor.try_with (fun () -> Beanstalk.default_connection ()) >>= function
    | Result.Ok x -> return x
    | Result.Error _ -> (print_endline "Failed to connect" ;exit 0)

let print_tubes conn =
  conn >>= (fun bs -> Beanstalk.Tube.all bs)
  >>= begin fun s ->
    List.iter s ~f:print_endline;
    return ()
  end

let print_stats conn ~tube =
  conn >>= (fun bs -> Beanstalk.Tube.stats bs ~tube) >>= begin fun stats->
    stats |> List.iter ~f:(fun (k, v) -> printf "%s = %s\n" k v);
    return ()
  end

let tube =
  Command.async_basic ~summary:"Inspect tubes"
    Command.Spec.(
      empty
      +> flag "-s" (optional_with_default "default" string)
          ~doc:"stats of a tube"
      +> flag "-l" (no_arg) ~doc:"list tubes"
    ) (fun tube list_tubes () ->
        if list_tubes then () |> conn |> print_tubes
        else print_stats (conn ()) ~tube)

let command =
  Command.group ~summary:"beanstalkd command line tool"
    ["tube", tube]

let () = Exn.handle_uncaught ~exit:true (fun () -> Command.run command)
