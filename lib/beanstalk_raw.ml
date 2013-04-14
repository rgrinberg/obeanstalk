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
  let read_rn_with_exn t = 
    read_line t >>| function
    | `Ok res -> Or_error.try_with (fun () -> (raise_if_error res; res))
    | `Eof -> failwith "unexpected eof"
end

type conn = BS of (Reader.t * Writer.t)

let default_port = 11300
let default_tube_name = "default"

let send (BS (_,w))  c = c |> wrap |> (Writer.write w)

let recv (BS (r, _)) =
  Reader.read_line r >>| function
  | `Ok res ->
    Or_error.try_with (fun () -> (raise_if_error res; res))
  | `Eof -> failwith "unexpected eof"

let log_output (BS (r, _)) = 
  let rec loop () =
    upon (Reader.read_line r) (function
      | `Ok query  -> print_endline query; loop ()
      | `Eof -> print_endline "DONE")
  in loop ()

let connect ~host ~port = 
  let where = Tcp.to_host_and_port host port in
  Tcp.connect where >>| (fun (_,reader, writer) -> BS (reader, writer))

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
    (Reader.read_line r) >>= function
    | `Eof -> assert false
    | `Ok buf -> 
      assert (String.length buf = size);
      Deferred.Or_error.return (cmd_reader cmd buf)

  let process cn ~req ~rep = 
    send cn req;
    match rep with (* ghettoish *)
    | (`Single _) as rep -> recv_single cn rep
    | (`WithPayload _) as rep -> recv_payload cn rep

  let process_k cn ~req ~rep ~k = (* FIXME *)
    let open Deferred.Or_error.Monad_infix in 
    process cn ~req ~rep >>| k 

  open Payload
  (* a little ugly since we don't parse jobs. but that function is set
   * by the user and it seems a little clumsy to pass it around when it
   * can just be applied to the result just as conveniently *)
  let parse_response : type a . a Payload.t -> a = function
  | YList x -> Yaml.to_list x
  | YDict x -> Yaml.to_dict x
  | Job x -> x 
end
