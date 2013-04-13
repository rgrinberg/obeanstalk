open Core.Std
open Async.Std

open Beanstalk_exc
open Beanstalk_cmd
open Beanstalk_raw

type conn = Beanstalk_raw.conn

(* TODO : not the mose useful data type, consider removing? *)
type conf = (string, string) List.Assoc.t

module type Serializable = sig
  type t
  val serialize : t -> string
  val deserialize : string -> t
  val size : t -> int
end

module type Job_intf = sig
  module S : Serializable
  type t
  val id : t -> int
  val data : t -> S.t
  val create : data:S.t -> id:int -> t
end

module MakeJob (S : Serializable) : Job_intf = struct
  module S = S
  type t = {
    id : int;
    data : S.t;
  }
  let id {id;_} = id
  let data {data;_} = data
  let create ~data ~id = {data ; id}
end

module Tube = struct

  let all cn = 
    let open Exp in
    send cn (Request.list_tubes);
    let open Deferred.Or_error.Monad_infix in
    (recv_payload cn Response.list_tubes_any) >>| parse_response

  let stats cn ~tube = request_get_yaml_dict cn 
      ~cmd:(Request.stats_tube ~name:tube) 
      ~resp_handler:(fun r -> `Ok (Response.stats_tube r))

  let pause cn ~tube ~delay = request_process_ignore cn
      ~cmd:(Request.pause_tube ~tube ~delay)
      ~process:(Response.pause_tube)

  let watched cn = 
    request_get_yaml_list cn ~cmd:(Request.list_tubes_watched)
      ~resp_handler:(fun r -> `Ok (Response.stats_tube r))

  let watch cn ~tube = request_process cn
      ~cmd:(Request.watch ~tube)
      ~process:(Response.watch)

  let ignore_tube cn ~tube = request_process_ignore cn 
      ~cmd:(Request.ignore_tube ~tube)
      ~process:(Response.ignore_tube)

  let use cn ~tube = 
    let open Exp in
    process_k cn 
      ~req:(Request.use_tube ~tube)
      ~rep:(Response.using)
      ~k:ignore |> extract `WithPayload

  let using cn =
    let open Exp in
    process cn
      ~req:(Request.list_tube_used)
      ~rep:(Response.using) |> extract `Single

end  

module Worker (S : Serializable) = struct
  module Job = MakeJob(S)
  type t = Job.t

  let reserve ?timeout cn = 
    let cmd = match timeout with
      | None -> Request.reserve
      | Some x -> Request.reserve_timeout ~timeout:x
    in let open Deferred.Or_error.Monad_infix in 
    (request_get_job cn ~cmd
       ~resp_handler:(fun r -> `Ok (Response.reserve r))) >>|
    (fun job -> Job.create ~data:(Job.S.deserialize job#job) ~id:(job#id))

  let put cn ?delay ~priority ~ttr ~job = 
    let data = Job.S.serialize job in
    let bytes = Job.S.size job in
    let open Deferred.Or_error.Monad_infix in 
    (request_with_job cn ~cmd:(Request.put ?delay ~priority ~ttr ~bytes)
       ~data ~process:(Response.put)) >>| 
    (fun (`Id id) -> Job.create ~id ~data:job)

  let bury cn ~id ~priority = request_process_ignore cn
      ~cmd:(Request.bury ~id ~priority)
      ~process:(Response.bury)

  let delete cn ~id = request_process_ignore cn
      ~cmd:(Request.delete ~id)
      ~process:(Response.delete)

  let touch cn ~id =  request_process_ignore cn
      ~cmd:(Request.touch ~id)
      ~process:(Response.touch)

  let release cn ~id ~priority ~delay = request_process_ignore cn
      ~cmd:(Request.release ~id ~priority ~delay)
      ~process:(Response.release)

  let peek_get_job cn ~peek =
    let open Deferred.Or_error.Monad_infix in
    (request_get_job cn ~cmd:peek
       ~resp_handler:(fun r -> `Ok (Response.peek_any r)))
    >>| (fun job -> Job.create ~data:(Job.S.deserialize job#job) ~id:(job#id))

  let peek cn ~id = peek_get_job cn ~peek:(Request.peek ~id)

  let peek_ready cn = peek_get_job cn ~peek:(Request.peek_ready)

  let peek_delayed cn = peek_get_job cn ~peek:(Request.peek_delayed)

  let peek_buried cn = peek_get_job cn ~peek:(Request.peek_buried)

  let kick_bound cn ~bound = request_process cn
      ~cmd:(Request.kick ~bound)
      ~process:(Response.kick)

  let kick_job cn ~id = request_process_ignore cn
      ~cmd:(Request.kick_job ~id)
      ~process:(Response.kick_job)

  let stats cn ~id = request_get_yaml_dict cn ~cmd:(Request.stats_job ~id)
      ~resp_handler:(fun resp -> `Ok(Response.stats_tube resp) )
end

let connect ?(port=default_port) ~host = 
  let where = Tcp.to_host_and_port host port in
  Tcp.connect where >>| (fun (_,reader, writer) -> (BS (reader, writer)))
