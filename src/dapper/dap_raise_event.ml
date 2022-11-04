module Res = Dap.Response
module Ev = Dap.Event

module type Types = sig

  type ev
  type body
  type pbody
  type ev_
  type body_
  type pbody_
end

module type Handler = sig

  include Types

  type in_msg
  type out_msg

  type t

  val make :
    handler: (in_msg Ev.t -> out_msg Ev.t Dap_result.t) ->
    ctor:(out_msg -> out_msg Ev.t) ->
    t

  val handle : t -> in_msg Ev.t -> out_msg Ev.t Dap_result.t

end



module WithSeqr (T:Types)
  : Handler
    with type ev := T.ev
     and type body := T.body
     and type pbody := T.pbody
     and type ev_ := T.ev_
     and type body_ := T.body_
     and type pbody_ := T.pbody_
     and type in_msg = (T.ev, T.body, T.pbody) Ev.EventMessage.t
     and type out_msg = (T.ev_, T.body_, T.pbody_) Ev.EventMessage.t
= struct

  type in_msg = (T.ev, T.body, T.pbody) Ev.EventMessage.t
  type out_msg = (T.ev_, T.body_, T.pbody_) Ev.EventMessage.t
  type t = {
    handler: in_msg Ev.t -> out_msg Ev.t Dap_result.t;
    ctor: out_msg -> out_msg Ev.t;
  }


  let make ~handler ~ctor = {handler; ctor}

  let wrapper ~ctor f =
    let getseq = Ev.(Fmap EventMessage.seq) in
    let setseq seq = Ev.(Fmap (fun ev ->
        EventMessage.set_seq ev ~seq
      )) in
    let setseq_err seq request_seq = Res.(Fmap (fun resp ->
        let resp = ResponseMessage.set_request_seq resp ~request_seq in
        ResponseMessage.set_seq resp ~seq
      ))
    in
    fun resp ->
      let request_seq = Ev.(eval @@ Map (Val getseq, Val resp)) in
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

  let handle {handler; ctor} in_msg =
    wrapper ~ctor handler in_msg

end
