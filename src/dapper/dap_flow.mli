
(* result-bind like behaviour from req -> resp, resp -> ev, ev -> ev *)
(* takes care of sequencing numbers correctly *)
(* and also the types for commands etc *)

type 'a t

val from_result : ('a, string) Result.t -> 'a t
val to_result : 'a t -> ('a, string) Result.t

val from_request :
  ('command, 'args, 'pargs) Dap_message.request ->
  ('command, 'args, 'pargs) Dap_message.request t

val from_response :
  ('command, 'body, 'pbody) Dap_message.response ->
  ('command, 'body, 'pbody) Dap_message.response t

val from_event :
  ('event, 'body, 'pbody) Dap_message.event ->
  ('event, 'body, 'pbody) Dap_message.event t

val to_request :
  ('command, 'args, 'presence) Dap_message.request ->
  ('command, 'args, 'presence) Dap_message.RequestMessage.t

val to_response :
  ('command, 'body, 'presence) Dap_message.response ->
  ('command, 'body, 'presence) Dap_message.ResponseMessage.t

val to_event :
  ('event, 'body, 'presence) Dap_message.event ->
  ('event, 'body, 'presence) Dap_message.EventMessage.t

(* NOTE in req_resp there is only one 'command type ie NextRequest -> NextResponse *)
val on_request :
  ('cmd, 'args, 'pargs) Dap_message.request t ->
  (('cmd, 'args, 'pargs) Dap_message.request -> ('cmd, 'body, 'pbody) Dap_message.response t) ->
  ('cmd, 'body, 'pbody) Dap_message.response t

val on_response :
  ('command, 'body, 'pbody) Dap_message.response t ->
  (('command, 'body, 'pbody) Dap_message.response -> ('event, 'evbody, 'pevbody) Dap_message.event t) ->
  ('event, 'evbody, 'pevbody) Dap_message.event t

val raise_event :
  ('event, 'evbody, 'pevbody) Dap_message.event t ->
  (('event, 'evbody, 'pevbody) Dap_message.event -> ('event_, 'evbody_, 'pevbody_) Dap_message.event t) ->
  ('event_, 'evbody_, 'pevbody_) Dap_message.event t
