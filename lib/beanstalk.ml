open Core.Std
open Async.Std

open Beanstalk_exc
open Beanstalk_cmd
open Beanstalk_raw

type conn = Beanstalk_raw.conn

module type Serializable = sig
  type t
  val serialize : t -> string
  val deserialize : string -> t
  val size : t -> int
end

module Job = struct
  type t = {
    id : int;
    data : string;
  } with sexp
  let id {id;_} = id
  let data {data;_} = data
  let create ~data ~id = {data ; id}
end

module Tube = struct
  open Beanstalk_raw

  let all cn = 
    process_k cn 
      ~req:(Request.list_tubes)
      ~rep:(Response.list_tubes_any)
      ~k:parse_response

  let stats cn ~tube =
    process_k cn
      ~req:(Request.stats_tube ~tube)
      ~rep:(Response.stats_tube)
      ~k:parse_response

  let pause cn ~tube ~delay = 
    process cn
      ~req:(Request.pause_tube ~tube ~delay)
      ~rep:(Response.pause_tube)

  let watched cn = 
    process_k cn
      ~req:(Request.list_tubes_watched)
      ~rep:(Response.list_tubes_any)
      ~k:parse_response 

  let watch cn ~tube =
    process cn
      ~req:(Request.ignore_tube ~tube)
      ~rep:(Response.ignore_tube) 

  let ignore_tube cn ~tube = 
    process_k cn
      ~req:(Request.ignore_tube ~tube)
      ~rep:(Response.ignore_tube)
      ~k:ignore 

  let use cn ~tube = 
    process_k cn 
      ~req:(Request.use_tube ~tube)
      ~rep:(Response.using)
      ~k:ignore 

  let using cn =
    process cn
      ~req:(Request.list_tube_used)
      ~rep:(Response.using) 
end  

module Worker = struct
  type job = Job.t
  open Beanstalk_raw

  let reserve ?timeout cn = 
    let req = match timeout with
      | None -> Request.reserve
      | Some timeout -> Request.reserve_timeout ~timeout
    in process_k cn
      ~req ~rep:(Response.reserve)
      ~k:(fun (`Id id, data) -> 
        Job.create ~id ~data:(data |> parse_response))

  let put cn ?delay ~priority ~ttr ~data = 
    let bytes = String.length data in
    process_k cn
      ~req:(Request.put ?delay ~priority ~ttr ~bytes ~job:data)
      ~rep:(Response.put)
      ~k:(fun (`Id id) -> Job.create ~id ~data)

  let bury cn ~id ~priority =
    process cn
      ~req:(Request.bury ~id ~priority)
      ~rep:(Response.bury) 

  let delete cn ~id = 
    process cn
      ~req:(Request.delete ~id)
      ~rep:(Response.delete) 

  let touch cn ~id =
    process cn
      ~req:(Request.touch ~id)
      ~rep:(Response.touch) 

  let release cn ~id ~priority ~delay =
    process cn
      ~req:(Request.release ~id ~priority ~delay)
      ~rep:(Response.release) 

  let peek_any cn ~req = 
    process_k cn
      ~req ~rep:(Response.peek_any)
      ~k:(fun (`Id id, data) -> 
        Job.create ~id ~data:(data |> parse_response))

  let peek cn ~id = peek_any cn ~req:(Request.peek ~id)

  let peek_ready cn = peek_any cn ~req:(Request.peek_ready)

  let peek_delayed cn = peek_any cn ~req:(Request.peek_delayed)

  let peek_buried cn = peek_any cn ~req:(Request.peek_buried)

  let kick_bound cn ~bound =
    process cn
      ~req:(Request.kick_bound ~bound)
      ~rep:(Response.kick_bound) 

  let kick_job cn ~id =
    process cn
      ~req:(Request.kick_job ~id)
      ~rep:(Response.kick_job) 

  let stats cn ~id = 
    process_k cn
      ~req:(Request.stats_job ~id)
      ~rep:(Response.stats_job)
      ~k:parse_response 
end

let connect            = Beanstalk_raw.connect
let default_connection = Beanstalk_raw.default_connection
let quit               = Beanstalk_raw.quit
