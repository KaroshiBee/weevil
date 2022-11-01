
(* result-bind like behaviour from req -> resp, resp -> ev, ev -> ev *)
(* takes care of sequencing numbers correctly *)
(* and also the types for commands etc *)

type 'a t

val from_result : ('a, string) Result.t -> 'a t

val from_request :
  ('command, 'args, 'pargs) Dap_message.request ->
  ('command, 'args, 'pargs) Dap_message.request t

val from_response :
  ('command, 'body, 'pbody) Dap_message.response ->
  ('command, 'body, 'pbody) Dap_message.response t

val from_event :
  ('event, 'body, 'pbody) Dap_message.event ->
  ('event, 'body, 'pbody) Dap_message.event t

val to_result : 'a t -> ('a, string) Result.t

val to_request :
  ('command, 'args, 'presence) Dap_message.request ->
  ('command, 'args, 'presence) Dap_message.RequestMessage.t

val to_response :
  ('command, 'body, 'presence) Dap_message.response ->
  ('command, 'body, 'presence) Dap_message.ResponseMessage.t

val to_event :
  ('event, 'body, 'presence) Dap_message.event ->
  ('event, 'body, 'presence) Dap_message.EventMessage.t

(* map and bind *)
val map_request :
    ('cmd, 'args, 'pargs) Dap_message.request t ->
    (('cmd, 'args, 'pargs) Dap_message.request -> 'a) ->
    'a t

val map_response :
    ('cmd, 'body, 'pbody) Dap_message.response t ->
    (('cmd, 'body, 'pbody) Dap_message.response -> 'a) ->
    'a t

val map_event :
    ('ev, 'body, 'pbody) Dap_message.event t ->
    (('ev, 'body, 'pbody) Dap_message.event -> 'a) ->
    'a t

val bind_request :
    ('cmd, 'args, 'pargs) Dap_message.request t ->
    (('cmd, 'args, 'pargs) Dap_message.request -> 'a t) ->
    'a t

val bind_response :
    ('cmd, 'body, 'pbody) Dap_message.response t ->
    (('cmd, 'body, 'pbody) Dap_message.response -> 'a t) ->
    'a t

val bind_event :
    ('ev, 'body, 'pbody) Dap_message.event t ->
    (('ev, 'body, 'pbody) Dap_message.event -> 'a t) ->
    'a t

(* specific bind functions representing typical flows - ie req/resp, resp/event etc *)
(* NOTE in req_resp there is only one 'command type ie NextRequest -> NextResponse *)
val request_response :
  ('cmd, 'args, 'pargs) Dap_message.request t ->
  (('cmd, 'args, 'pargs) Dap_message.request -> ('cmd, 'body, 'pbody) Dap_message.response t) ->
  ('cmd, 'body, 'pbody) Dap_message.response t

val response_event :
  ('command, 'body, 'pbody) Dap_message.response t ->
  (('command, 'body, 'pbody) Dap_message.response -> ('event, 'evbody, 'pevbody) Dap_message.event t) ->
  ('event, 'evbody, 'pevbody) Dap_message.event t

val raise_event :
  ('event, 'evbody, 'pevbody) Dap_message.event t ->
  (('event, 'evbody, 'pevbody) Dap_message.event -> ('event_, 'evbody_, 'pevbody_) Dap_message.event t) ->
  ('event_, 'evbody_, 'pevbody_) Dap_message.event t

val raise_error :
  ('cmd, 'args, 'pargs) Dap_message.request t ->
  (
    ('cmd, 'args, 'pargs) Dap_message.request ->
    (Dap_commands.error, Dap_message.ErrorResponse_body.t, Dap_base.Presence.req) Dap_message.response t
  ) ->
  (Dap_commands.error, Dap_message.ErrorResponse_body.t, Dap_base.Presence.req) Dap_message.response t
