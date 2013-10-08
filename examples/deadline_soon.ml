(* reproduce the Deadline_soon exception *)
open Core.Std
open Async.Std
open Async_unix.Async_print

module B = Beanstalk

let bs = B.connect ~port:11300 ~host:"127.0.0.1"

let print_job job = printf "Job: %d, %s\n" (B.Job.id job) (B.Job.data job)

let r () =
  let rc () = Char.of_int_exn (Char.to_int 'b' + (Random.int 20))
  in List.init 5 ~f:(fun _ -> rc ()) |> String.of_char_list

let rec loop bs =
  let rec create_jobs = function
    | 0 -> return ()
    | i -> B.Worker.put bs ~delay:0 ~priority:1 ~ttr:3 ~data:(r ()) >>=
      (fun _ -> printf "created job\n"; create_jobs (pred i))
  in 
  let rec trigger_deadline_soon () =
    B.Worker.reserve bs >>=
    (fun job -> print_job job; return ()) >>=
    (fun _ -> Clock.after (sec 0.1)) >>=
    trigger_deadline_soon
  in create_jobs 3 >>= (fun _ -> trigger_deadline_soon ())

let _ = bs >>= loop

let () = never_returns (Scheduler.go ())
