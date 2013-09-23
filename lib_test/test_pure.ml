open OUnit
open Core.Std
open Beanstalk_cmd

let sample_yaml = "---
current-jobs-urgent: 0
current-jobs-ready: 0
current-jobs-reserved: 0
current-jobs-delayed: 0
current-jobs-buried: 0
cmd-put: 0
cmd-peek: 0
cmd-peek-ready: 0
cmd-peek-delayed: 0
cmd-peek-buried: 0
cmd-reserve: 0
cmd-reserve-with-timeout: 0
cmd-delete: 0
cmd-release: 0
cmd-use: 0
cmd-watch: 0
cmd-ignore: 0
cmd-bury: 0
cmd-kick: 0
cmd-touch: 0
cmd-stats: 1
cmd-stats-job: 0
cmd-stats-tube: 0
cmd-list-tubes: 0
cmd-list-tube-used: 0
cmd-list-tubes-watched: 0
cmd-pause-tube: 0
job-timeouts: 0
total-jobs: 0
max-job-size: 65535
current-tubes: 1
current-connections: 1
current-producers: 0
current-workers: 0
current-waiting: 0
total-connections: 1
pid: 13297"

let sample_yaml2 = "---
- default"

(* this was a bug *)
let sample_yaml3 = "---
- default
"

let id x = x

let test_split () =
  let (k,v) = Yaml.split_exn "test: 123" ~on:':' in
  assert_equal k "test" ~printer:id;
  assert_equal v " 123" ~printer:id

let test_yaml_dict () = 
  let yaml = Yaml.to_dict sample_yaml in
  assert_bool "more than 10" ((List.length yaml) > 10);
  assert_equal (List.Assoc.find_exn yaml "pid") "13297";
  assert_equal (List.Assoc.find_exn yaml "cmd-pause-tube") "0"

let test_yaml_list () = 
  let yaml = Yaml.to_list sample_yaml2 in
  assert_equal (List.hd_exn yaml) "default" ~printer:id

let test_list_bug () = 
  let yaml = Yaml.to_list sample_yaml2 in
  assert_equal (List.hd_exn yaml) "default" ~printer:id

let test_fixtures =
  "test yaml parsing" >:::
    [
      "test split" >:: test_split;
      "sample yaml dict" >:: test_yaml_dict;
      "sample yaml list" >:: test_yaml_list;
      "sample list bug" >:: test_list_bug;
    ]

let _ = run_test_tt ~verbose:true test_fixtures
