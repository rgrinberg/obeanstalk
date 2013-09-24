open Core.Std
open Async.Std
open Async_unix.Async_print

let readme () = "Example:
obean blah blah"

let conn () = Obeanstalk.connect ~port:11300 ~host:"127.0.0.1"

let tube =
  Command.async_basic ~summary:"Inspect tubes"
    Command.Spec.(
      empty
    ) (fun () -> 
      conn () >>= (fun bs -> Obeanstalk.Tube.all bs)
      >>= begin fun res ->
        begin match res with
          | Result.Ok s -> begin 
              print_endline "tubes:"; 
              List.iter s ~f:print_endline
            end
          | Result.Error _ -> print_endline "uh oh" end;
        return ()
      end)

let command =
  Command.group ~summary:"beanstalkd command line tool"
  ["tube", tube]

let () = Exn.handle_uncaught ~exit:true (fun () -> Command.run command)
