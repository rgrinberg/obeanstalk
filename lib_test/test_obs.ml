open OUnit
open Core.Std
open Async.Std
open Async_unix.Std
open Async_unix.Async_print
open Beanstalk_raw
let (host, port) = ("127.0.0.1", 11300)

let pf = Core.Std.Printf.printf

let verify_beanstalkd_on ~host ~port = 
  let failure () = 
    pf "Beanstalkd instance not running on %s:%d. Aborting\n" host port;
    shutdown 0 in 
  let success () = 
    pf "Beantalkd instance running on %s:%d. Running tests\n" host port in 
  Beanstalk_raw.health_check ~host ~port >>= (fun res ->
      (match res with | Ok `Ok -> success () | Error _ -> failure ());
      return (Ok ()))

let bsd_running () = 
  (verify_beanstalkd_on ~host ~port) >>> begin fun _ ->
    assert_bool "" true;
  end

let create_job () = 
  let open Beanstalk in
  let job_load = "sheni" in
  let bs = connect ~port ~host in 
  bs >>> begin fun bs ->
    let put_id = ref (-100) in
    (Worker.put bs ?delay:None ~priority:1 ~ttr:10 ~data:job_load) >>|
    begin fun job ->
      assert_equal (Job.data job) job_load;
      assert_bool "id exists" ((Job.id job) > 0);
      put_id := (Job.id job);
      pf "created job\n";
      return ()
    end >>= (fun _ ->
        Worker.reserve bs >>| begin fun job ->
          assert_equal (Job.data job) job_load;
          assert_equal (Job.id job) (!put_id);
          pf "reserved job...\n";
          return ()
        end) |> ignore
  end

let test_fixtures =
  "test beanstalk_raw" >:::
    [
      "beanstalkd is running" >:: bsd_running;
      "create job" >:: create_job
    ]

(*let _ = run_test_tt ~verbose:true test_fixtures*)

(*let _ = (after (sec 2.)) >>> (fun _ -> shutdown 0)*)

(*let () = never_returns (Scheduler.go ())*)
