open OUnit
open Core.Std
open Async.Std
open Async_unix.Std
open Async_unix.Async_print
open Beanstalk_raw
let (host, port) = ("127.0.0.1", 11300)

let verify_beanstalkd_on ~host ~port = 
  let pf = Printf.printf in 
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

let test_fixtures =
  "test beanstalk_raw" >:::
    [
      "beanstalkd is running" >:: bsd_running;
    ]

let _ = run_test_tt ~verbose:true test_fixtures

let _ = (after (sec 1.)) >>> (fun _ -> shutdown 0)

let () = never_returns (Scheduler.go ())
