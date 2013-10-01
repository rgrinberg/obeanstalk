open Core.Std
open Async.Std
open Async_unix.Async_print

module B = Beanstalk

let bs = B.connect ~port:11300 ~host:"127.0.0.1"

let print_job job = printf "Job: %d, %s\n" (B.Job.id job) (B.Job.data job)

let job_ready = Ivar.create ()

let res bs =
  B.Worker.reserve bs >>= fun job ->
  B.Worker.delete bs ~id:(B.Job.id job) >>= (fun _ ->
      print_job job; return ())

let () = 
  Ivar.read job_ready >>> begin fun bs ->
    (res bs) >>= (fun _ -> res bs) >>> fun _ -> (shutdown 0)
  end

let create_random_job bs =
  let r () =
    let rc () = Char.of_int_exn (Char.to_int 'b' + (Random.int 20))
    in List.init 5 ~f:(fun _ -> rc ()) |> String.of_char_list in
  Beanstalk.Worker.put bs ~delay:0 ~priority:2 ~ttr:10 ~data:(r ())

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

