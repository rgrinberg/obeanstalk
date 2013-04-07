open OUnit
open Core.Std
open Beanstalk_cmd


let sample_yaml = ""

let test_yaml () = 
  assert_bool "TODO" true

let test_fixtures =
  "test yaml parsing" >:::
    [
      "beanstalkd is running" >:: test_yaml;
    ]

let _ = run_test_tt ~verbose:true test_fixtures
