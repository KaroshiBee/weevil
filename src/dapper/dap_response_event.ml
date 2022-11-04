module Res = Dap.Response
module Ev = Dap.Event

module type Types = sig

  type cmd
  type body
  type pbody
  type ev
  type body_
  type pbody_
end

module type Handler = sig

  include Types

  type res_msg
  type ev_msg

  type t

  val make :
    handler: (res_msg Res.t -> ev_msg Ev.t Dap_result.t) ->
    ctor:(ev_msg -> ev_msg Ev.t) ->
    t

  val handle : t -> res_msg Res.t -> ev_msg Ev.t Dap_result.t

end

module WithSeqr (T:Types)
  : Handler
  with type cmd := T.cmd
   and type body := T.body
   and type pbody := T.pbody
   and type ev := T.ev
   and type body_ := T.body_
   and type pbody_ := T.pbody_
   and type res_msg = (T.cmd, T.body, T.pbody) Res.ResponseMessage.t
   and type ev_msg = (T.ev, T.body_, T.pbody_) Ev.EventMessage.t
= struct

  type res_msg = (T.cmd, T.body, T.pbody) Res.ResponseMessage.t
  type ev_msg = (T.ev, T.body_, T.pbody_) Ev.EventMessage.t
  type t = {
    handler: res_msg Res.t -> ev_msg Ev.t Dap_result.t;
    ctor: ev_msg -> ev_msg Ev.t;
  }

  let make ~handler ~ctor = {handler; ctor}

  let wrapper ~ctor f =
    let getseq = Res.(Fmap ResponseMessage.seq) in
    let setseq seq = Ev.(Fmap (fun ev ->
        EventMessage.set_seq ev ~seq
      )) in
    let setseq_err seq request_seq = Res.(Fmap (fun resp ->
        let resp = ResponseMessage.set_request_seq resp ~request_seq in
        ResponseMessage.set_seq resp ~seq
      ))
    in
    fun resp ->
      let request_seq = Res.(eval @@ Map (Val getseq, Val resp)) in
      let seq = 1 + request_seq in
      let ev = f resp in
      Dap_result.bind ev (fun v-> Ev.(
          eval @@ Map (Val (setseq seq), Val v)
          |> ctor
          |> Dap_result.ok
        ))
      |> Dap_result.map_error (fun err-> Res.(
          eval @@ Map (Val (setseq_err seq request_seq), Val err)
          |> errorResponse
        ))



  let handle {handler; ctor} res =
    wrapper ~ctor handler res

end
