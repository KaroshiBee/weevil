module Req = Dap.Request
module Res = Dap.Response


module type Types = sig

  type cmd
  type args
  type pargs
  type body
  type pbody

end

module type Handler = sig

  include Types

  type in_msg
  type out_msg

  type t

  val make :
    handler: (in_msg Req.t -> out_msg Res.t Dap_result.t) ->
    ctor:(out_msg -> out_msg Res.t) ->
    t

  val handle : t -> in_msg Req.t -> out_msg Res.t Dap_result.t

end

module WithSeqr (T : Types) :
  Handler
  with type cmd := T.cmd
   and type args := T.args
   and type pargs := T.pargs
   and type body := T.body
   and type pbody := T.pbody
   and type in_msg = (T.cmd, T.args, T.pargs) Req.RequestMessage.t
   and type out_msg = (T.cmd, T.body, T.pbody) Res.ResponseMessage.t
= struct
  (* NOTE the cmd param is the same for both the request and the response *)
  type in_msg = (T.cmd, T.args, T.pargs) Req.RequestMessage.t
  type out_msg = (T.cmd, T.body, T.pbody) Res.ResponseMessage.t
  type t = {
    handler: in_msg Req.t -> out_msg Res.t Dap_result.t;
    ctor: out_msg -> out_msg Res.t;
  }

  let make ~handler ~ctor = {handler; ctor}

  let wrapper ~ctor f =
    let getseq = Req.(Fmap RequestMessage.seq) in
    let setseq seq request_seq = Res.(Fmap (fun msg ->
        let msg = ResponseMessage.set_request_seq msg ~request_seq in
        ResponseMessage.set_seq msg ~seq
      ))
    in
    fun msg ->
      let request_seq = Req.(eval @@ Map (Val getseq, Val msg)) in
      let seq = 1 + request_seq in
      let msg = f msg in
      Dap_result.bind msg (fun v-> Res.(
          eval @@ Map (Val (setseq seq request_seq), Val v)
          |> ctor
          |> Dap_result.ok
        ))
      |> Dap_result.map_error (fun err-> Res.(
          eval @@ Map (Val (setseq seq request_seq), Val err)
          |> errorResponse
        ))



  let handle {handler; ctor} req =
    wrapper ~ctor handler req

end
