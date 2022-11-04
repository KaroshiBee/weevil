module type Types = sig

  type ev
  type body
  type pbody
  type ev_
  type body_
  type pbody_

end

module In = Dap.Event
module Out = Dap.Event
module Err = Dap.Response

module type Handler = sig

  include Types

  type in_msg
  type out_msg

  type t

  val make :
    handler: (in_msg In.t -> out_msg Out.t Dap_result.t) ->
    ctor:(out_msg -> out_msg Out.t) ->
    t

  val handle : t -> in_msg In.t -> out_msg Out.t Dap_result.t

end



module WithSeqr (T:Types)
  : Handler
    with type ev := T.ev
     and type body := T.body
     and type pbody := T.pbody
     and type ev_ := T.ev_
     and type body_ := T.body_
     and type pbody_ := T.pbody_
     and type in_msg = (T.ev, T.body, T.pbody) In.EventMessage.t
     and type out_msg = (T.ev_, T.body_, T.pbody_) Out.EventMessage.t
= struct

  type in_msg = (T.ev, T.body, T.pbody) In.EventMessage.t
  type out_msg = (T.ev_, T.body_, T.pbody_) Out.EventMessage.t
  type t = {
    handler: in_msg In.t -> out_msg Out.t Dap_result.t;
    ctor: out_msg -> out_msg Out.t;
  }


  let make ~handler ~ctor = {handler; ctor}

  let wrapper ~ctor f =
    let getseq = In.(Fmap EventMessage.seq) in
    let setseq seq = Out.(Fmap (EventMessage.set_seq ~seq)) in
    let setseq_err seq request_seq = Err.(Fmap (fun msg ->
        msg
        |> ResponseMessage.set_request_seq ~request_seq
        |> ResponseMessage.set_seq ~seq
      ))
    in
    fun msg ->
      let request_seq = In.(eval @@ Map (Val getseq, Val msg)) in
      let seq = 1 + request_seq in
      let msg = f msg in
      Dap_result.bind msg (fun v-> Out.(
          eval @@ Map (Val (setseq seq), Val v)
          |> ctor
          |> Dap_result.ok
        ))
      |> Dap_result.map_error (fun err-> Err.(
          eval @@ Map (Val (setseq_err seq request_seq), Val err)
          |> errorResponse
        ))

  let handle {handler; ctor} in_msg =
    wrapper ~ctor handler in_msg

end
