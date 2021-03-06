# Obeanstalk

Obeanstalk is an Async client for the beanstalk work queue.

## Documentation - Coming "soon":

In all seriousness though `make doc` will create standard ocamldoc
documentation. There should be enough stuff in `examples/` and `lib_test/` to
get started. The module should be completely straightforward if you've ever
worked with beanstalk before. In the meantime, checkout `lib/beanstalk.mli`.

## Example
Leasing jobs off a tube
```
open Core.Std
open Async.Std
module B = Beanstalk

let bs = B.connect ~port:111300 ~host:"0.0.0.0"

let rec loop bs =
  Monitor.try_with begin fun () ->
    B.Worker.reserve bs >>= (fun job ->
        let id = B.Job.id job in
        printf "Leased job: %d\n" id;
        B.Worker.delete bs ~id) >>= fun () ->
    loop bs
  end >>| function
  | Ok _ -> assert false (* infinite loop *)
  | Error (B.Beanstalk_error s) ->
      let error = s |> B.sexp_of_error |> Sexp.to_string_hum in
      printf "failed because: %s\n" error
  | Error _ -> shutdown (-1) (* don't care about other errors *)


let _ = bs >>= loop

let () = never_returns (Scheduler.go ())

```

## Wishlist
* Lwt Support
* "debug mode" to log all interaction with a beanstalkd instance
