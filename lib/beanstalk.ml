open Core.Std
open Async.Std

type conn = Beanstalk_raw.conn

open Beanstalk_exc

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
  open Beanstalk_cmd

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
      ~rep:Response.using
      ~k:ignore 

  let using cn =
    process cn
      ~req:Request.list_tube_used
      ~rep:Response.using
end  

module Worker = struct
  open Beanstalk_raw
  open Beanstalk_cmd

  let reserve_timeout cn span =
    let seconds = span |> Time.Span.to_sec |> Int.of_float in
    Monitor.try_with ~extract_exn:true (fun () -> 
        process_k cn
          ~req:(Request.reserve_timeout ~timeout:seconds) ~rep:Response.reserve
          ~k:(fun (`Id id, data) -> Job.create ~id ~data:(parse_response data))
      ) >>| function
    | Ok x -> `Ok x
    | Error Timed_out -> `Timed_out
    | Error x -> raise x

  let reserve_now cn = reserve_timeout cn Time.Span.zero

  let reserve cn =
    process_k cn ~req:Request.reserve ~rep:Response.reserve
      ~k:(fun (`Id id, data) -> Job.create ~id ~data:(parse_response data))

  let put ?delay ?priority ?ttr cn ~data =
    let bytes = String.length data in
    process_k cn
      ~req:(Request.put ?delay ?priority ?ttr ~bytes ~job:data)
      ~rep:(Response.put)
      ~k:(fun (`Id id) -> Job.create ~id ~data)

  let bury cn ~id ~priority =
    (* burying is a little special because BURIED is an error in most
     * other requests. But here it's the expected response *)
    Monitor.try_with ~extract_exn:true (fun () ->
        process cn
          ~req:(Request.bury ~id ~priority) ~rep:Response.bury
      ) >>| function
    | Ok x -> failwith "should not happen"
    | Error (Beanstalk_error(Buried (None))) -> ()
    | Error x -> raise x

  let delete cn ~id = 
    process cn
      ~req:(Request.delete ~id)
      ~rep:Response.delete

  let touch cn ~id =
    process cn
      ~req:(Request.touch ~id)
      ~rep:Response.touch

  let release cn ~id ~priority ~delay =
    process cn
      ~req:(Request.release ~id ~priority ~delay)
      ~rep:Response.release

  let peek_any cn ~req = 
    process_k cn
      ~req ~rep:Response.peek_any
      ~k:(fun (`Id id, data) -> 
          Job.create ~id ~data:(parse_response data))

  let peek cn ~id = peek_any cn ~req:(Request.peek ~id)

  let peek_ready cn = peek_any cn ~req:Request.peek_ready

  let peek_delayed cn = peek_any cn ~req:Request.peek_delayed

  let peek_buried cn = peek_any cn ~req:Request.peek_buried

  let kick_bound cn ~bound =
    process cn
      ~req:(Request.kick_bound ~bound)
      ~rep:Response.kick_bound

  let kick cn ~id =
    process cn
      ~req:(Request.kick_job ~id)
      ~rep:Response.kick_job

  let stats cn ~id = 
    process_k cn
      ~req:(Request.stats_job ~id)
      ~rep:Response.stats_job
      ~k:parse_response 
end

let connect            = Beanstalk_raw.connect
let default_connection = Beanstalk_raw.default_connection
let quit               = Beanstalk_raw.quit

include Beanstalk_exc
