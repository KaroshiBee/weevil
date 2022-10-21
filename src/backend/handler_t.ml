open Dapper.Dap_message
module Dap_commands = Dapper.Dap_commands
module Dap_events = Dapper.Dap_events
module Dap_flow = Dapper.Dap_flow


type launch_mode = [`Launch | `Attach | `AttachForSuspendedLaunch]

type config = {launch_mode : launch_mode}

let default_response_req
  = fun command body ->
  (* NOTE for use in the Flow monad so seq and request_seq get taken care of there *)
  let seq = -1 in
  let request_seq = -1 in
  let success = true in
  ResponseMessage.make
    ~seq
    ~request_seq
    ~success
    ~command
    ~body
    ()

let default_response_opt
  = fun command body ->
  (* NOTE for use in the Flow monad so seq and request_seq get taken care of there *)
  let seq = -1 in
  let request_seq = -1 in
  let success = true in
  ResponseMessage.make_opt
    ~seq
    ~request_seq
    ~success
    ~command
    ~body
    ()

let default_event_req
  = fun event body ->
  (* NOTE for use in the Flow monad so seq and request_seq get taken care of there *)
  let seq = -1 in
  EventMessage.make
    ~seq
    ~event
    ~body
    ()

let default_event_opt
  = fun event body ->
  (* NOTE for use in the Flow monad so seq and request_seq get taken care of there *)
  let seq = -1 in
  EventMessage.make_opt
    ~seq
    ~event
    ~body
    ()


(*
Previous handler func:
  client:Lwt_io.output Lwt_io.channel ->
  msg:string ->
  backend:Lwt_io.output Lwt_io.channel ->
  unit Lwt.t
*)
module type HANDLER = sig

  type input
  type output

  type backend_channel

  val from_string : string -> input
  (* NOTE when handling a request we will be interacting with the backend, hence the Lwt.t  *)
  val handle : config:config -> input -> output Lwt.t
  val to_string : output -> (string, string) Result.t Lwt.t

end

module type REQ_T = sig
  type ('command, 'args, 'presence) t
  type command
  val command : command Dap_commands.t
  type args
  type presence
  val enc : (command, args, presence) t Data_encoding.t
  val ctor : (command, args, presence) t -> (command, args, presence) request
  val extract : (command, args, presence) request -> (command, args, presence) t
end

module type RESP_T = sig
  type ('command, 'body, 'presence) t
  type command
  val command : command Dap_commands.t
  type body
  type presence
  val enc : (command, body, presence) t Data_encoding.t
  val ctor : (command, body, presence) t -> (command, body, presence) response
  val extract : (command, body, presence) response -> (command, body, presence) t
end

module type EV_T = sig
  type ('event, 'body, 'presence) t
  type event
  val event : event Dap_events.t
  type body
  type presence
  val enc : (event, body, presence) t Data_encoding.t
  val ctor : (event, body, presence) t -> (event, body, presence) Dapper.Dap_message.event
  val extract : (event, body, presence) Dapper.Dap_message.event -> (event, body, presence) t
end

module MakeReqRespIncludes
    (REQ:REQ_T)
    (RESP:RESP_T with type command = REQ.command)
    = struct

  type req = (REQ.command, REQ.args, REQ.presence) request
  type input = req Dap_flow.t

  type resp = (RESP.command, RESP.body, RESP.presence) response

  type output = {
    response: resp Dap_flow.t;
  }

  type backend_channel = Lwt_io.output_channel

  let from_string =
    fun input ->
      Js_msg.from_string input
      |> Result.map (Js_msg.destruct REQ.enc)
      |> Result.map (fun x -> REQ.ctor x)
      |> Dap_flow.from_result

  let to_string =
    fun {response; } ->
      match Dap_flow.(to_result response) with
      | Result.Ok response ->
        let resp = RESP.extract response |> Js_msg.construct RESP.enc |> Js_msg.to_string in
        Result.ok resp
        |> Lwt.return

      | Result.Error _ ->
        failwith "TODO - response errored, need to make an error response str from the initial request seq#"

end


module MakeReqRespIncludes_withEvent
    (REQ:REQ_T)
    (RESP:RESP_T with type command = REQ.command)
    (EV:EV_T)
    = struct

  type req = (REQ.command, REQ.args, REQ.presence) request
  type input = req Dap_flow.t

  type resp = (RESP.command, RESP.body, RESP.presence) response
  type ev = (EV.event, EV.body, EV.presence) event

  type output = {
    response: resp Dap_flow.t;
    event: ev Dap_flow.t option;
  }

  type backend_channel = Lwt_io.output_channel

  let from_string =
    fun input ->
      Js_msg.from_string input
      |> Result.map (Js_msg.destruct REQ.enc)
      |> Result.map (fun x -> REQ.ctor x)
      |> Dap_flow.from_result

  let to_string = function
    | {response; event=Some event} -> (
      match Dap_flow.(to_result response, to_result event) with
      | Result.Ok response, Result.Ok event ->
        let resp = RESP.extract response |> Js_msg.construct RESP.enc |> Js_msg.to_string in
        let ev = EV.extract event |> Js_msg.construct EV.enc |> Js_msg.to_string in
        Result.ok @@ resp^ev
        |> Lwt.return

      | Result.Error _, Result.Ok (InitializedEvent _) ->
        failwith "TODO - response errored, need to make an error response str from the initial request seq#"
      | Result.Ok (InitializeResponse _resp), Result.Error _ ->
        failwith "TODO - response ok, but couldn't raise the event??"
      | Result.Error _, Result.Error _ ->
        failwith "TODO - nothing worked"
      | _, _ -> assert false
    )

    | {response; event=None} -> (
      match Dap_flow.(to_result response) with
      | Result.Ok response ->
        let resp = RESP.extract response |> Js_msg.construct RESP.enc |> Js_msg.to_string in
        Result.ok @@ resp
        |> Lwt.return

      | Result.Error _ ->
        failwith "TODO - response errored, need to make an error response str from the initial request seq#"
    )

end


