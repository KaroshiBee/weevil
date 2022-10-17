
(* result-bind like behaviour from req -> resp, resp -> ev, ev -> ev *)
(* takes care of sequencing numbers correctly *)
(* and also the types for commands etc *)
(* TODO error message ids need to be unique *)
(* NOTE using ErrorResponse_body here because wont know the sequence number *)
(*      of the error message until it is ready to be sent *)

type 'a t = ('a, Dap_message.ErrorResponse_body.t) Result.t
type ('command, 'args, 'pargs) req  = ('command, 'args, 'pargs) Dap_t.RequestMessage.t
type ('command, 'body, 'pbody) resp = ('command, 'body, 'pbody) Dap_t.ResponseMessage.t
type ('event, 'body, 'pbody) ev = ('event, 'body, 'pbody) Dap_t.EventMessage.t

(* NOTE in req_resp there is only one 'command type ie NextRequest -> NextResponse *)
val req_resp :
  ('command, 'args, 'pargs) req t ->
  (('command, 'args, 'pargs) req -> ('command, 'body, 'pbody) resp t) ->
  ('command, 'body, 'pbody) resp t

val resp_ev :
  ('command, 'body, 'pbody) resp t ->
  (('command, 'body, 'pbody) resp -> ('event, 'evbody, 'pevbody) ev t) ->
  ('event, 'evbody, 'pevbody) ev t

val next_ev :
  ('event, 'evbody, 'pevbody) ev t ->
  (('event, 'evbody, 'pevbody) ev -> ('event_, 'evbody_, 'pevbody_) ev t) ->
  ('event_, 'evbody_, 'pevbody_) ev t
