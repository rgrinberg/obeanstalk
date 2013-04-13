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
    process_k cn 
      ~req:(Request.list_tubes)
      ~rep:(Response.list_tubes_any)
      ~k:parse_response |> extract `WithPayload

  let stats cn ~tube =
    let open Exp in
    process_k cn
      ~req:(Request.stats_tube ~tube)
      ~rep:(Response.stats_tube)
      ~k:parse_response |> extract `WithPayload

  let pause cn ~tube ~delay = 
    let open Exp in
    process cn
      ~req:(Request.pause_tube ~tube ~delay)
      ~rep:(Response.pause_tube) |> extract `Single

  let watched cn = 
    let open Exp in
    process_k cn
      ~req:(Request.list_tubes_watched)
      ~rep:(Response.list_tubes_any)
      ~k:parse_response |> extract `WithPayload

  let watch cn ~tube =
    let open Exp in
    process cn
      ~req:(Request.ignore_tube ~tube)
      ~rep:(Response.ignore_tube) |> extract `Single

  let ignore_tube cn ~tube = 
    let open Exp in
    process_k cn
      ~req:(Request.ignore_tube ~tube)
      ~rep:(Response.ignore_tube)
      ~k:ignore |> extract `Single

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
    let open Exp in
    let req = match timeout with
      | None -> Request.reserve
      | Some timeout -> Request.reserve_timeout ~timeout
    in process_k cn
      ~req ~rep:(Response.reserve)
      ~k:(fun (`Id id, data) -> 
        Job.create ~id ~data:(data |> parse_response |> Job.S.deserialize))
       |> extract `WithPayload

  let put cn ?delay ~priority ~ttr ~job = 
    let open Exp in 
    let data = Job.S.serialize job in
    let bytes = Job.S.size job in
    process_k cn
      ~req:(Request.put ?delay ~priority ~ttr ~bytes ~job:data)
      ~rep:(Response.put)
      ~k:(fun (`Id id, j) -> Job.create ~id ~data:job) (* fix stupid naming *)
    |> extract `WithPayload

  let bury cn ~id ~priority =
    let open Exp in
    process cn
      ~req:(Request.bury ~id ~priority)
      ~rep:(Response.bury) |> extract `Single

  let delete cn ~id = 
    let open Exp in
    process cn
      ~req:(Request.delete ~id)
      ~rep:(Response.delete) |> extract `Single

  let touch cn ~id =
    let open Exp in
    process cn
      ~req:(Request.touch ~id)
      ~rep:(Response.touch) |> extract `Single

  let release cn ~id ~priority ~delay =
    let open Exp in
    process cn
      ~req:(Request.release ~id ~priority ~delay)
      ~rep:(Response.release) |> extract `Single

  let peek_get_job cn ~peek =
    let open Deferred.Or_error.Monad_infix in
    (request_get_job cn ~cmd:peek
       ~resp_handler:(fun r -> `Ok (Response.peek_any r)))
    >>| (fun job -> Job.create ~data:(Job.S.deserialize job#job) ~id:(job#id))

  let peek cn ~id = peek_get_job cn ~peek:(Request.peek ~id)

  let peek_ready cn = peek_get_job cn ~peek:(Request.peek_ready)

  let peek_delayed cn = peek_get_job cn ~peek:(Request.peek_delayed)

  let peek_buried cn = peek_get_job cn ~peek:(Request.peek_buried)

  let kick_bound cn ~bound =
    let open Exp in
    process cn
      ~req:(Request.kick_bound ~bound)
      ~rep:(Response.kick_bound) |> extract `Single

  let kick_job cn ~id =
    let open Exp in
    process cn
      ~req:(Request.kick_job ~id)
      ~rep:(Response.kick_job) |> extract `Single

  let stats cn ~id = request_get_yaml_dict cn ~cmd:(Request.stats_job ~id)
      ~resp_handler:(fun resp -> `Ok(Response.stats_job resp) )
end

let connect ?(port=default_port) ~host = 
  let where = Tcp.to_host_and_port host port in
  Tcp.connect where >>| (fun (_,reader, writer) -> (BS (reader, writer)))
