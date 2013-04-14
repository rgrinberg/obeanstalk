open Core.Std
open Async.Std
open Async_unix.Async_print

module O = Obeanstalk
module S = Obeanstalk.Stringly

let pe = print_endline
let pf = printf

let bs = O.connect ~port:11300 ~host:"127.0.0.1"

let () = 
  bs >>> fun bs ->
  S.Worker.reserve bs >>> (fun job -> 
    match job with
    | Result.Ok job ->
      let data = job |> S.Worker.Job.data |> S.Worker.Job.S.serialize in
      pf "Received job (%d): %s" (S.Worker.Job.id job) data
    | Result.Error _ -> pe "uh oh")

let () = 
  bs >>> fun bs -> 
  pe "connected to beanstalkd server";
  S.Worker.put bs ~delay:0 ~priority:2 ~ttr:10 
    ~job:(S.Worker.Job.S.deserialize "shen sheni") >>> fun job_e ->
  pe "Received result";
  begin match job_e with
    | Result.Ok job -> pf "Created job with id: %d\n" (S.Worker.Job.id job)
    | Result.Error e -> pe "uh oh"
  end;
  shutdown 0

let () = never_returns (Scheduler.go ())

