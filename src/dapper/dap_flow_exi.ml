open Dap_message_exi

let from_result x = x
let to_result x = x

let from_request = Result.ok
let from_response = Result.ok
let from_event = Result.ok
(* is there a way of lifting
   (cmd, _, _) RequestMessage.t -> (cmd, _, _) ResponseMessage.t
   into
   request -> response
   ?
*)
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


module RR = Dap_request_response

module Launch = RR.WithSeqr (struct
  type cmd = Dap_commands.launch
  type args = LaunchRequestArguments.t
  type pargs = Dap_base.Presence.req
  type body = EmptyObject.t option
  type pbody = Dap_base.Presence.opt
  type t = {
    f: (cmd, args, pargs) RequestMessage.t -> (cmd, body, pbody) ResponseMessage.t Dap_result.t;
  }

  let make f = {f;}

  let handle {f;} = function
    | LaunchRequest req -> f req |> Result.map launchResponse
    | _ -> Result.error @@ default_response_error "wrong request"

end)


module Attach = RR.WithSeqr (struct
  type cmd = Dap_commands.attach
  type args = AttachRequestArguments.t
  type pargs = Dap_base.Presence.req
  type body = EmptyObject.t option
  type pbody = Dap_base.Presence.opt
  type t = {
    f: (cmd, args, pargs) RequestMessage.t -> (cmd, body, pbody) ResponseMessage.t Dap_result.t;
  }

  let make f = {f;}

  let handle {f;} = function
    | AttachRequest req -> f req |> Result.map attachResponse
    | _ -> Result.error @@ default_response_error "wrong request"

end)

module Completions = RR.WithSeqr (struct
  type cmd = Dap_commands.completions
  type args = CompletionsArguments.t
  type pargs = Dap_base.Presence.req
  type body = CompletionsResponse_body.t
  type pbody = Dap_base.Presence.req
  type t = {
    f: (cmd, args, pargs) RequestMessage.t -> (cmd, body, pbody) ResponseMessage.t Dap_result.t;
  }

  let make f = {f;}

  let handle {f;} = function
    | CompletionsRequest req -> f req |> Result.map completionsResponse
    | _ -> Result.error @@ default_response_error "wrong request"

end)

let completion_handler =
  let c = Completions.make (fun _ ->
      let cmpl = CompletionsResponse_body.make ~targets:[] () in
      default_response_req Dap_commands.completions cmpl |> Result.ok
    )
  in
  Completions.handle c

let req_attach = attachRequest @@ RequestMessage.make ~seq:0 ~command:Dap_commands.attach ~arguments:(AttachRequestArguments.make ()) ()
let req_launch = launchRequest @@ RequestMessage.make ~seq:0 ~command:Dap_commands.launch ~arguments:(LaunchRequestArguments.make ()) ()
let req_completion = completionsRequest @@ RequestMessage.make ~seq:0 ~command:Dap_commands.completions
       ~arguments:(CompletionsArguments.make ~text:"" ~column:0 ()) ()


