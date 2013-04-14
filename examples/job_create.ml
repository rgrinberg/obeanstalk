open Core.Std
open Async.Std
open Async_unix.Async_print

module O = Obeanstalk
module S = Obeanstalk.Stringly

let pe = print_endline
let pf = printf

let bs = O.connect ~port:11300 ~host:"127.0.0.1"

let job_ready = Ivar.create ()

let () = 
  Ivar.read job_ready >>> fun bs ->
  S.Worker.reserve bs >>> begin fun job -> 
    begin match job with
      | Result.Ok job ->
        let data = S.Worker.Job.data job in
        pf "Received job (%d): '%s'" (S.Job.id job) data;
      | Result.Error _ -> pe "uh oh";
    end;
    upon (after (sec 0.2)) (fun _ -> shutdown 0)
  end

let () = 
  bs >>> fun bs -> 
  pe "connected to beanstalkd server";
  S.Worker.put bs ~delay:0 ~priority:2 ~ttr:10 
    ~job:"shen sheni" >>> fun job_e ->
  pe "Received result";
  begin match job_e with
    | Result.Ok job -> pf "Created job with id: %d\n" (S.Worker.Job.id job)
    | Result.Error e -> pe "uh oh"
  end;
  Ivar.fill job_ready bs

let () = never_returns (Scheduler.go ())

