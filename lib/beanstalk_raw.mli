open Core.Std
open Async.Std

type conn = BS of Reader.t * Writer.t

val default_port : int

val default_tube_name : string
val send : conn -> string -> unit
val recv : conn -> string Deferred.t
val connect : host:string -> port:int -> conn Deferred.t
val connect_host : host:string -> conn Deferred.t

val health_check :
  host:string ->
  port:int -> ([> `Ok ], exn) Result.t Deferred.t

module Exp : sig
  val send : conn -> Beanstalk_cmd.Request.t -> unit
  val recv_single :
    conn ->
    [< `Single of Beanstalk_cmd.Command.t -> 'a ] ->
    'a Deferred.t
  val recv_payload :
    conn ->
    [< `WithPayload of Beanstalk_cmd.Command.t -> string -> 'a ] ->
    'a Deferred.t
  val process :
    conn ->
    req:Beanstalk_cmd.Request.t ->
    rep:[< `Single of Beanstalk_cmd.Command.t -> 'a
        | `WithPayload of Beanstalk_cmd.Command.t -> string -> 'a ] ->
    'a Deferred.t
  val process_k :
    conn ->
    req:Beanstalk_cmd.Request.t ->
    rep:[< `Single of Beanstalk_cmd.Command.t -> 'a
        | `WithPayload of Beanstalk_cmd.Command.t -> string -> 'a ] ->
    k:('a -> 'b) -> 'b Deferred.t
  val parse_response : 'a Beanstalk_cmd.Payload.t -> 'a
end
