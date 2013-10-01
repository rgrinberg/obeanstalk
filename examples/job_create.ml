open Core.Std
open Async.Std
open Async_unix.Async_print

let bs = Beanstalk.connect ~port:11300 ~host:"127.0.0.1"

let job_ready = Ivar.create ()

let () = 
  Ivar.read job_ready >>> fun bs ->
  Beanstalk.Worker.reserve bs >>> begin fun job -> 
    let data = Beanstalk.Job.data job in
    printf "Received job (%d): '%s'" (Beanstalk.Job.id job) data;
    upon (after (sec 0.2)) (fun _ -> shutdown 0)
  end

let () = 
  bs >>> fun bs -> 
  print_endline "connected to beanstalkd server";
  (Beanstalk.Worker.put bs ~delay:0 ~priority:2 ~ttr:10 
     ~data:"shen sheni") >>> (fun job_e ->
      print_endline "Received result";
      printf "Created job with id: %d\n" (Beanstalk.Job.id job_e);
      Ivar.fill job_ready bs)

let () = never_returns (Scheduler.go ())

