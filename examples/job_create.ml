open Core.Std
open Async.Std

module O = Obeanstalk
module S = Obeanstalk.Stringly

let pe = Caml.print_endline
let pf = Caml.Printf.printf

let () = 
  (O.connect ~port:11300 ~host:"127.0.0.1") >>> fun bs -> 
  pe "connected to beanstalkd server";
  S.Worker.put bs ~delay:0 ~priority:2 ~ttr:10 
  ~job:(S.Worker.Job.S.deserialize "shen sheni") >>> fun job_e ->
    match job_e with
    | Result.Ok job -> pf "Created job with id: %d\n" (S.Worker.Job.id job)
    | Result.Error e -> pe "uh oh"

let () = never_returns (Scheduler.go ())

