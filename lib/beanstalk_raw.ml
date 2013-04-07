open Core.Std
open Async.Std
open Beanstalk_exc
open Beanstalk_cmd

(** This is the extremely primitive interface to beanstalkd instance it 
 *  almost nothing but sending and receiving commands and translating errors.
 *  Use the higher level interface that comes with this package instead
 * *)

type conn = BS of (Reader.t * Writer.t)

let default_port = 11300
let default_tube_name = "default"

let send (BS (_,w))  c = c |> wrap |> (Writer.write w)

let recv (BS (r, _)) =
  Reader.read_line r >>| function
  | `Ok res ->
    Or_error.try_with (fun () -> (raise_if_error res; unwrap res))
  | `Eof -> failwith "unexpected eof"

(* TODO : get rid of the extra string allocation *)
let read_len (BS (r, _)) ~len = 
  let buf = String.create (len+2) in
  (Reader.really_read r buf) >>| function
    | `Eof _ -> assert false
    | `Ok -> buf |> String.sub ~pos:0 ~len

let send_with_data (BS (_, w)) ~cmd ~data = 
  let (cmd, data) = (wrap cmd, wrap data) in
  (cmd ^ data) |> Writer.write w

let send_batch t ~cmds = cmds |> List.iter ~f:(fun cmd -> send t cmd)

let request bs ~cmd = send bs cmd; recv bs

let request_pair bs ~cmd = 
  send bs cmd;
  let open Deferred.Monad_infix in
  (recv bs) >>= (fun r1 -> recv bs >>| (fun r2 -> (r1, r2)))

let request_batch bs ~cmds = send_batch bs ~cmds; recv bs

let request_pair_batch bs ~cmds =
  send_batch bs ~cmds;
  let open Deferred.Or_error.Monad_infix in
  (recv bs) >>= (fun r1 -> recv bs >>| (fun r2 -> (r1, r2)))

let log_output (BS (r, _)) = 
  let rec loop () =
    upon (Reader.read_line r) (function
      | `Ok query  -> print_endline query; loop ()
      | `Eof -> print_endline "DONE")
  in loop ()

let connect ~host ~port = 
  let where = Tcp.to_host_and_port host port in
  Tcp.connect where >>| (fun (_,reader, writer) ->
    BS (reader, writer))

let connect_host ~host = 
  connect ~port:default_port ~host

let health_check ~host ~port =
  Monitor.try_with ~extract_exn:true begin fun () ->
    connect ~port ~host >>= (fun ((BS (r, w)) as bs) ->
      send bs "stats";
      Reader.read_line r >>| function
      | `Ok res ->
        if (String.sub ~pos:0 ~len: 2 res = "OK") then `Ok
        else raise (Unexpected_response (res))
      | `Eof -> failwith "Unexpected eof")
  end

let request_with ~f ~process = 
  (f ()) >>| (fun e -> 
    let open Or_error.Monad_infix in
    e >>= (fun resp -> Response.(try_with process ~resp)))

let request_process cn ~cmd ~process = 
  request_with ~f:(fun e -> request cn ~cmd) ~process

let request_process_ignore cn ~cmd ~process = 
  let open Deferred.Or_error.Monad_infix in 
  (request_process cn ~cmd ~process) >>| (fun _ -> ())

let request_with_job cn ~cmd ~data ~process =
  let f = (fun () -> send_with_data cn ~cmd ~data; recv cn) in
  request_with ~f ~process

let request_with_len : conn -> cmd:string -> _ = 
  fun cn ~cmd ~length ->
    let open Deferred.Or_error.Monad_infix in
    (request cn ~cmd) >>= fun resp ->
    match length resp with
    | `Error -> failwith "TODO"
    | `Ok (len) ->
      let open Deferred.Monad_infix in
      (* TODO : Clean this up *)
      (read_len cn ~len) >>= fun job -> job |> Deferred.Or_error.return

let request_get_job cn ~cmd ~resp_handler =
  (* TODO : this handling is a little big ugly. Fix later *)
  let id = ref None in
  let length s = match resp_handler s with
  | `Error -> failwith "TODO"
  | `Ok (`Id i, `Bytes len) -> (id := (Some i); `Ok(len)) in
  let open Deferred.Or_error.Monad_infix in 
  (request_with_len cn ~cmd ~length) >>| (fun job -> object
    method job = job
    method id = Option.value_exn (!id)
  end)
