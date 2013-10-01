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

let create_random_job bs =
  let r () =
    let rc () = Char.of_int_exn (Char.to_int 'b' + (Random.int 20))
    in List.init 5 ~f:(fun _ -> rc ()) |> String.of_char_list in
  Beanstalk.Worker.put bs ~delay:0 ~priority:2 ~ttr:10 ~data:(r ())

let print_job job = printf "Created job with id: %d\n" (Beanstalk.Job.id job)

(*let () = *)
  (*bs >>> begin fun bs -> *)
    (*print_endline "connected to beanstalkd server";*)
    (*Deferred.all [create_random_job bs; create_random_job bs] >>> fun jobs ->*)
    (*jobs |> List.iter ~f:print_job;*)
    (*Ivar.fill job_ready bs*)
  (*end*)

let () = 
  bs >>> begin fun bs -> 
    print_endline "connected to beanstalkd server";
    create_random_job bs >>> fun j1 ->
    create_random_job bs >>> fun j2 ->
    [j1; j2] |> List.iter ~f:print_job;
    Ivar.fill job_ready bs
  end


let () = never_returns (Scheduler.go ())

