open Dap_message

let default_response_req ?(success=true)
  = fun command body ->
  (* NOTE for use in the Flow monad so seq and request_seq get taken care of there *)
  let seq = -1 in
  let request_seq = -1 in
  ResponseMessage.make
    ~seq
    ~request_seq
    ~success
    ~command
    ~body
    ()

let default_response_opt ?(success=true)
  = fun command body ->
  (* NOTE for use in the Flow monad so seq and request_seq get taken care of there *)
  let seq = -1 in
  let request_seq = -1 in
  ResponseMessage.make_opt
    ~seq
    ~request_seq
    ~success
    ~command
    ~body
    ()

let default_response_error e =
  let id = Hashtbl.hash e in
  let variables = `O [("error", `String e)] in
  let error = Message.make ~id ~format:"{error}" ~variables () in
  let body = ErrorResponse_body.make ~error () in
  default_response_req ~success:false Dap_commands.error body

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
module Backend = struct
  type io = Lwt_io.input_channel * Lwt_io.output_channel
  type t = {
    mutable process: Lwt_process.process_full option; (* the backend svc process, None if attaching to already running one *)
    mutable io: io option;
    callback: Lwt_process.process_full -> unit Lwt.t;
  }

  let make_empty f = {
    process=None; io=None; callback=f;
  }
  let process_full t = t.process
  let set_process_full t process = t.process <- Some process
  let callback t = t.callback

  let ic t = t.io |> Option.map fst
  let oc t = t.io |> Option.map snd
  let set_io t ic oc = t.io <- Some (ic, oc)

end

module type HANDLER = sig

  type t
  val make_empty : (Lwt_process.process_full -> unit Lwt.t) -> t
  val process_full : t -> Lwt_process.process_full option
  val set_process_full : t -> Lwt_process.process_full -> unit
  val callback : t -> (Lwt_process.process_full -> unit Lwt.t)

  val ic : t -> Lwt_io.input_channel option
  val oc : t -> Lwt_io.output_channel option
  val set_io : t -> Lwt_io.input_channel -> Lwt_io.output_channel -> unit


  type input
  type output
  val string_to_input : string -> input
  (* NOTE when handling a request we will be interacting with the backend, hence the Lwt.t  *)
  val handle : t -> Dap_config.t -> input -> output Lwt.t
  val output_to_string : output -> (string, string) Result.t Lwt.t

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
  val ctor : (event, body, presence) t -> (event, body, presence) Dap_message.event
  val extract : (event, body, presence) Dap_message.event -> (event, body, presence) t
end

type error = (Dap_commands.error, ErrorResponse_body.t, ResponseMessage.req) response

module MakeReqRespIncludes
    (REQ:REQ_T)
    (RESP:RESP_T with type command = REQ.command) = struct

  include Backend

  type req = (REQ.command, REQ.args, REQ.presence) request
  type input = req Dap_flow.t

  type resp = (RESP.command, RESP.body, RESP.presence) response

  type output = {
    response: resp Dap_flow.t;
    error: error Dap_flow.t option;
  }

  let string_to_input =
    fun input ->
      Dap_js_msg.from_string input
      |> Result.map (Dap_js_msg.destruct REQ.enc)
      |> Result.map (fun x -> REQ.ctor x)
      |> Dap_flow.from_result

  let output_to_string = 
    fun {response; _} ->
      match Dap_flow.(to_result response) with
      | Result.Ok response ->
        let resp = RESP.extract response |> Dap_js_msg.construct RESP.enc |> Dap_js_msg.to_string in
        Result.ok resp
        |> Lwt.return

      | Result.Error e -> Result.error e |> Lwt.return
        (* let id = Hashtbl.hash e in *)
        (* let variables = `O [("error", `String e)] in *)
        (* let error = Message.make ~id ~format:"{error}" ~variables () in *)
        (* let body = ErrorResponse_body.make ~error () in *)
        (* let err = RESP.make ~seq:0 ~request_seq:0 ~success:false ~command:Dap_commands.error ~body () in *)
        (* let err_resp = err |> Dap_js_msg.construct RESP.enc |> Dap_js_msg.to_string in *)
        (* Result.Error err_resp *)
        (* |> Lwt.return *)

end


module MakeReqRespIncludes_withEvent
    (REQ:REQ_T)
    (RESP:RESP_T with type command = REQ.command)
    (EV:EV_T)
    = struct

  include Backend

  type req = (REQ.command, REQ.args, REQ.presence) request
  type input = req Dap_flow.t

  type resp = (RESP.command, RESP.body, RESP.presence) response
  type ev = (EV.event, EV.body, EV.presence) event

  type output = {
    response: resp Dap_flow.t;
    event: ev Dap_flow.t option;
    error: error Dap_flow.t option;
  }

  let string_to_input =
    fun input ->
      Dap_js_msg.from_string input
      |> Result.map (Dap_js_msg.destruct REQ.enc)
      |> Result.map (fun x -> REQ.ctor x)
      |> Dap_flow.from_result

  let output_to_string = function
    | {response; event=Some event; _} -> (
      match Dap_flow.(to_result response, to_result event) with
      | Result.Ok response, Result.Ok event ->
        let resp = RESP.extract response |> Dap_js_msg.construct RESP.enc |> Dap_js_msg.to_string in
        let ev = EV.extract event |> Dap_js_msg.construct EV.enc |> Dap_js_msg.to_string in
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

    | {response; event=None; _} -> (
      match Dap_flow.(to_result response) with
      | Result.Ok response ->
        let resp = RESP.extract response |> Dap_js_msg.construct RESP.enc |> Dap_js_msg.to_string in
        Result.ok @@ resp
        |> Lwt.return

      | Result.Error _ ->
        failwith "TODO - response errored, need to make an error response str from the initial request seq#"
    )

end


