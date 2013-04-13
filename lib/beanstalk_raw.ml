open Core.Std
open Async.Std
open Beanstalk_exc
open Beanstalk_cmd

(** This is the extremely primitive interface to beanstalkd instance it 
 *  almost nothing but sending and receiving commands and translating errors.
 *  Use the higher level interface that comes with this package instead
 * *)

module Writer = struct
  include Writer
  let write_rn ?pos ?len writer str =
    Writer.write ?pos ?len writer str;
    Writer.write writer "\r\n"
end
module Reader = struct
  include Reader
  let read_rn t = 
    (Reader.read_line t) >>| function
    (* we will live with this slight inefficiency for now *)
    | `Ok res -> `Ok(String.(sub ~pos:0 ~len:(res |> length |> pred) res))
    | `Eof -> `Eof
  let read_rn_with_exn t = 
    read_rn t >>| function
    | `Ok res -> Or_error.try_with (fun () -> (raise_if_error res; res))
    | `Eof -> failwith "unexpected eof"
end

type conn = BS of (Reader.t * Writer.t)

let default_port = 11300
let default_tube_name = "default"

let send (BS (_,w))  c = c |> wrap |> (Writer.write w)

let recv (BS (r, _)) =
  Reader.read_rn r >>| function
  | `Ok res ->
    Or_error.try_with (fun () -> (raise_if_error res; res))
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

(* independent call from everything else, mostly a sanity check *)
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
    | `Ok (`Bytes len) ->
      let open Deferred.Monad_infix in
      (* TODO : Clean this up *)
      (read_len cn ~len) >>= (fun job -> job |> Deferred.Or_error.return)

let request_get_yaml_dict cn ~cmd ~resp_handler = 
  let open Deferred.Or_error.Monad_infix in 
  (request_with_len cn ~cmd ~length:resp_handler) >>| Yaml.to_dict

let request_get_yaml_list cn ~cmd ~resp_handler = 
  let open Deferred.Or_error.Monad_infix in 
  (request_with_len cn ~cmd ~length:resp_handler) >>| Yaml.to_list

let request_get_job cn ~cmd ~resp_handler =
  (* TODO : this handling is a little big ugly. Fix later *)
  let id = ref None in
  let length s = match resp_handler s with
    | `Error -> failwith "TODO"
    | `Ok (`Id i, len) -> (id := (Some i); `Ok(len)) in
  let open Deferred.Or_error.Monad_infix in 
  (request_with_len cn ~cmd ~length) >>| (fun job -> object
    method job = job
    method id = Option.value_exn (!id)
  end)


(* Standalone request routines, independent of almost all of the rest of the code
 * Uses the new {Request,Response,Command} module stuff for somewhat
 * cleaner and more typesafe handling. *)
module Exp = struct
  let send (BS (r,w)) req = 
    let open Request in match req with
    | Single cmd -> Writer.write_rn w (Command.to_string cmd)
    | WithJob (cmd, load) -> begin
        Writer.write_rn w (Command.to_string cmd);
        Writer.write_rn w load
      end

  let recv_single (BS (r, _)) (`Single cmd_reader) = 
    let open Deferred.Or_error.Monad_infix in 
    (Reader.read_rn_with_exn r) >>|
    fun s -> s |> Command.of_string |> cmd_reader

  let recv_payload (BS (r, _)) (`WithPayload cmd_reader) = 
    let open Deferred.Or_error.Monad_infix in 
    (Reader.read_rn_with_exn r) >>= fun str_cmd ->
    let cmd = Command.of_string str_cmd in
    let size = Command.size cmd in 
    let open Deferred.Monad_infix in
    (Reader.read_rn r) >>= function
    | `Eof -> assert false
    | `Ok buf -> 
      assert (String.length buf = size);
      Deferred.Or_error.return (cmd_reader cmd buf)

  (* TODO : fix the ugly *)
  (* I have a feeling all of this extract bs can be fixed with use of GADT's
   * but don't how to make it work exactly *)
  let process cn ~req ~rep = 
    send cn req;
    match rep with (* ghettoish *)
    | (`Single _) as rep -> `Single (recv_single cn rep)
    | (`WithPayload _) as rep -> `WithPayload (recv_payload cn rep)

  let process_k cn ~req ~rep ~k = (* FIXME *)
    let open Deferred.Or_error.Monad_infix in 
    match process cn ~req ~rep with
    | `Single x -> `Single (x >>| k)
    | `WithPayload x -> `WithPayload (x >>| k)

  let extract tag value = (* FIXME *)
    match tag, value with
    | `Single, (`Single x) -> x
    | `WithPayload, (`WithPayload x) -> x
    | _ , _ -> assert false

  open Payload
  (* a little ugly since we don't parse jobs. but that function is set
   * by the user and it seems a little clumsy to pass it around when it
   * can just be applied to the result just as conveniently *)
  let parse_response : type a . a Payload.t -> a = function
    | YList x -> Yaml.to_list x
    | YDict x -> Yaml.to_dict x
    | Job x -> x 
end
