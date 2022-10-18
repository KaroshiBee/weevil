
(* result-bind like behaviour from req -> resp, resp -> ev, ev -> ev *)
(* takes care of sequencing numbers correctly *)
(* and also the types for commands etc *)
(* TODO error message ids need to be unique *)
(* NOTE using ErrorResponse_body here because wont know the sequence number *)
(*      of the error message until it is ready to be sent *)

type 'a t = ('a, Dap_message.ErrorResponse_body.t) Result.t

val of_req :
  ('command, 'args, 'pargs) Dap_message.request ->
  ('command, 'args, 'pargs) Dap_message.request t

(* NOTE in req_resp there is only one 'command type ie NextRequest -> NextResponse *)
val req_resp :
  ('cmd, 'args, 'pargs) Dap_message.request t ->
  (('cmd, 'args, 'pargs) Dap_message.request -> ('cmd, 'body, 'pbody) Dap_message.response t) ->
  ('cmd, 'body, 'pbody) Dap_message.response t

val resp_ev :
  ('command, 'body, 'pbody) Dap_message.response t ->
  (('command, 'body, 'pbody) Dap_message.response -> ('event, 'evbody, 'pevbody) Dap_message.event t) ->
  ('event, 'evbody, 'pevbody) Dap_message.event t

val next_ev :
  ('event, 'evbody, 'pevbody) Dap_message.event t ->
  (('event, 'evbody, 'pevbody) Dap_message.event -> ('event_, 'evbody_, 'pevbody_) Dap_message.event t) ->
  ('event_, 'evbody_, 'pevbody_) Dap_message.event t
