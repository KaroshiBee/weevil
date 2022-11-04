module type Types = sig

  type cmd
  type body
  type pbody
  type ev
  type body_
  type pbody_
end

module In = Dap.Response
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
  with type cmd := T.cmd
   and type body := T.body
   and type pbody := T.pbody
   and type ev := T.ev
   and type body_ := T.body_
   and type pbody_ := T.pbody_
   and type in_msg = (T.cmd, T.body, T.pbody) In.Message.t
   and type out_msg = (T.ev, T.body_, T.pbody_) Out.Message.t
= struct

  type in_msg = (T.cmd, T.body, T.pbody) In.Message.t
  type out_msg = (T.ev, T.body_, T.pbody_) Out.Message.t
  type t = {
    handler: in_msg In.t -> out_msg Out.t Dap_result.t;
    ctor: out_msg -> out_msg Out.t;
  }

  let make ~handler ~ctor = {handler; ctor}

  let wrapper ~ctor f =
    let getseq = In.(Fmap Message.seq) in
    let setseq seq = Out.(Fmap (Message.set_seq ~seq)) in
    let setseq_err seq request_seq = Err.(Fmap (fun msg ->
        msg
        |> Message.set_request_seq ~request_seq
        |> Message.set_seq ~seq
      ))
    in
    fun msg ->
      let request_seq = In.(eval @@ Map (Val getseq, Val msg)) in
      let seq = 1 + request_seq in
      f msg
        |> Dap_result.bind ~f:(fun v-> Out.(
          eval @@ Map (Val (setseq seq), Val v)
          |> ctor
          |> Dap_result.ok
        ))
        |> Dap_result.map_error ~f:(fun err-> Err.(
          eval @@ Map (Val (setseq_err seq request_seq), Val err)
          |> errorResponse
        ))

  let handle {handler; ctor} = wrapper ~ctor handler

end
