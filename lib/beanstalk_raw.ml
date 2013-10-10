open Core.Std
open Async.Std
module Prot = Beanstalk_cmd
module Exc  = Beanstalk_exc

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
  (** read line, throw exception if on beanstalkd error *)
  let read_rn_with_exn t = 
    read_line t >>| function
    | `Ok res -> (Prot.raise_if_error res; res)
    | `Eof -> failwith "unexpected eof"

  (* inefficient version? calls read twice *)
  let read_buffer_drop_rn' r ~len =
    let buf = String.create len in
    really_read r ~len buf >>= function
    | `Ok -> really_read r ~len:2 (String.create 2) >>| (fun _ -> `Ok buf)
    | `Eof x -> return (`Eof x)

  (** read a string of len [len] otherwise pass the error. drop \r\n
   * after reading [len] bytes*)
  let read_buffer_drop_rn r ~len =
    let len = len + 2 in
    let buf = String.create len in
    really_read r ~len buf >>| function
      | `Ok -> `Ok (Prot.unwrap buf)
      | `Eof x -> `Eof x
end

module Conn = struct
  type t = {
    reader : Reader.t;
    writer : Writer.t;
    throttle : unit Throttle.Sequencer.t
  }

  let create ~reader ~writer = { 
    reader; writer;
    throttle=(Throttle.Sequencer.create ~continue_on_error:true ());
  }
  let enqueue {reader;writer;throttle} ~f =
    Throttle.enqueue throttle (fun () -> f reader writer)
end

type conn = Conn.t
open Conn

let default_port = 11300
let default_tube_name = "default"

let connect ~host ~port = 
  let where = Tcp.to_host_and_port host port in
  Tcp.connect where >>| (fun (_,reader, writer) -> Conn.create ~reader ~writer)

let default_connection ?(host="localhost") ?(port=default_port) () =
  connect ~host ~port

let quit {reader=r;writer=w;_} =
  Writer.close w >>= (fun _ -> Reader.close r)

(* independent call from everything else, mostly a sanity check *)
let health_check ~host ~port =
  Monitor.try_with ~extract_exn:true begin fun () ->
    connect ~port ~host >>= (fun {reader=r;writer=w;_} ->
        "stats" |> Prot.wrap |> Writer.write w;
        Reader.read_line r >>| function
        | `Ok res ->
          if (String.sub ~pos:0 ~len: 2 res = "OK") then `Ok
          else raise (Exc.Unexpected_response (res))
        | `Eof -> failwith "Unexpected eof")
  end

let send {writer=w;_} req = 
  let open Prot.Request in match req with
  | Single cmd -> Writer.write_rn w (Prot.Command.to_string cmd)
  | WithJob (cmd, load) -> begin
      Writer.write_rn w (Prot.Command.to_string cmd);
      Writer.write_rn w load
    end

let recv_single {reader=r;_} (`Single cmd_reader) = 
  (Reader.read_rn_with_exn r) >>|
  fun s -> s |> Prot.Command.of_string |> cmd_reader

let recv_payload {reader=r;_} (`WithPayload cmd_reader) = 
  (Reader.read_rn_with_exn r) >>= fun str_cmd ->
  let cmd = Prot.Command.of_string str_cmd in
  let size = Prot.Command.size cmd in 
  Reader.read_buffer_drop_rn r ~len:size >>= function
  | `Ok buf -> return (cmd_reader cmd buf)
  | `Eof _ -> assert false (* TODO *)

let process cn ~req ~rep = 
  Conn.enqueue cn ~f:(fun _ _ ->
      send cn req;
      match rep with
      | (`Single _) as rep -> recv_single cn rep
      | (`WithPayload _) as rep -> recv_payload cn rep)

let process_k cn ~req ~rep ~k = process cn ~req ~rep >>| k 

open Prot.Payload
(* a little ugly since we don't parse jobs. but that function is set
  * by the user and it seems a little clumsy to pass it around when it
  * can just be applied to the result just as conveniently *)
let parse_response : type a . a Prot.Payload.t -> a = function
  | YList x -> Yaml.to_list x
  | YDict x -> Yaml.to_dict x
  | Job x -> x 
