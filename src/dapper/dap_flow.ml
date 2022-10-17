open Dap_t


type 'a t = ('a, Dap_message.ErrorResponse_body.t) Result.t
type ('command, 'args, 'pargs) req  = ('command, 'args, 'pargs) RequestMessage.t
type ('command, 'body, 'pbody) resp = ('command, 'body, 'pbody) ResponseMessage.t
type ('event, 'body, 'pbody) ev = ('event, 'body, 'pbody) EventMessage.t

let of_req :
  ('command, 'args, 'pargs) req ->
  ('command, 'args, 'pargs) req t
    = Result.ok

let req_resp :
  (('command, 'args, 'pargs) req t ->
   (('command, 'args, 'pargs) req -> ('command, 'body, 'pbody) resp t) ->
   ('command, 'body, 'pbody) resp t) = fun v f ->
  match v with
  | Result.Ok req ->
    let request_seq = RequestMessage.seq req in
    let seq = succ @@ request_seq in
    let resp = f req in
    Result.bind resp (fun resp ->
        let resp = ResponseMessage.set_request_seq resp request_seq in
        let resp = ResponseMessage.set_seq resp seq in
        (Result.Ok resp)
      )
  | Result.Error _ as err -> err

let resp_ev :
  ('command, 'body, 'pbody) resp t ->
  (('command, 'body, 'pbody) resp -> ('event, 'evbody, 'pevbody) ev t) ->
  ('event, 'evbody, 'pevbody) ev t = fun v f ->
  match v with
  | Result.Ok resp ->
    let request_seq = ResponseMessage.seq resp in
    let seq = succ @@ request_seq in
    let ev = f resp in
    Result.bind ev (fun ev ->
        let ev = EventMessage.set_seq ev seq in
        (Result.Ok ev)
      )
  | Result.Error _ as err -> err


let next_ev :
  ('event, 'evbody, 'pevbody) ev t ->
  (('event, 'evbody, 'pevbody) ev -> ('event_, 'evbody_, 'pevbody_) ev t) ->
  ('event_, 'evbody_, 'pevbody_) ev t = fun v f ->
  match v with
  | Result.Ok ev ->
    let request_seq = EventMessage.seq ev in
    let seq = succ @@ request_seq in
    let ev = f ev in
    Result.bind ev (fun ev ->
        let ev = EventMessage.set_seq ev seq in
        (Result.Ok ev)
      )
  | Result.Error _ as err -> err


