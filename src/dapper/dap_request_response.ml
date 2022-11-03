module Req = Dap_utils.Request
module Res = Dap_utils.Response


module type Types = sig
  type cmd

  type args

  type pargs

  type body

  type pbody
end

module type Handler = sig

  include Types

  type req_msg (* = (cmd, args, pargs) Req.Message.t *)
  type res_msg (* = (cmd, body, pbody) Res.Message.t *)

  type t

  val make :
    handler: (req_msg Req.t -> res_msg Res.t Dap_result.t) ->
    ctor:(res_msg -> res_msg Res.t) ->
    t
  (* val make : *)
  (*   handler: ((cmd, args, pargs) Req.Message.t Req.t -> (cmd, body, pbody) Res.Message.t Res.t Dap_result.t) -> *)
  (*   ctor:((cmd, body, pbody) Res.Message.t -> (cmd, body, pbody) Res.Message.t Res.t) -> *)
  (*   t *)

  (* NOTE the cmd param is the same for both the request and the response *)
  val handle : t -> req_msg Req.t -> res_msg Res.t Dap_result.t

end

module WithSeqr (T : Types) :
  Handler
  with type cmd := T.cmd
   and type args := T.args
   and type pargs := T.pargs
   and type body := T.body
   and type pbody := T.pbody
   and type req_msg = (T.cmd, T.args, T.pargs) Req.RequestMessage.t
   and type res_msg = (T.cmd, T.body, T.pbody) Res.ResponseMessage.t
= struct
  type req_msg = (T.cmd, T.args, T.pargs) Req.RequestMessage.t
  type res_msg = (T.cmd, T.body, T.pbody) Res.ResponseMessage.t
  type t = {
    handler: req_msg Req.t -> res_msg Res.t Dap_result.t;
    ctor: res_msg -> res_msg Res.t;
  }

  let make ~handler ~ctor = {handler; ctor}

  let wrapper ~ctor f =
    let getseq = Req.(Fmap RequestMessage.seq) in
    let setseq seq request_seq = Res.(Fmap (fun resp ->
        let resp = ResponseMessage.set_request_seq resp ~request_seq in
        ResponseMessage.set_seq resp ~seq
      ))
    in
    fun req ->
      let request_seq = Req.(eval @@ Map (Val getseq, Val req)) in
      let seq = 1 + request_seq in
      let resp = f req in
      Dap_result.bind resp (fun v->
          Res.(eval @@ Map (Val (setseq seq request_seq), Val v))
          |> ctor
          |> Dap_result.ok
        )
      |> Dap_result.map_error (fun err-> Res.(
               eval @@ Map (Val (setseq seq request_seq), Val err)
               |> errorResponse
             ))



  let handle {handler; ctor} req =
    let f = wrapper ~ctor handler in
    f req
end
