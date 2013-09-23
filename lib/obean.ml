open Core.Std
open Async.Std

let readme () = "Example:
obean blah blah"

let command =
  Command.async_basic ~summary:"Test client for beanstalkd" ~readme
    Command.Spec.(
      empty
      +> flag "-tl" (no_arg) ~doc:"List tubes"
      +> flag "-c" (no_arg) ~doc:"List consumers"
    ) (fun tubes consumers () -> 
        Obeanstalk.connect ~port:11300 ~host:"127.0.0.1" >>=
        (fun bs -> Obeanstalk.Tube.all bs) >>= begin fun res ->
          begin match res with
            | Result.Ok s -> begin 
                print_endline "tubes:"; 
                List.iter s ~f:print_endline
              end
            | Result.Error _ -> print_endline "uh oh" end;
          Clock.after (sec 2.0)
        end)

let () = Exn.handle_uncaught ~exit:true (fun () -> Command.run command)
