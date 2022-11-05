module type Types = sig
  type cmd

  type args

  type pargs

  type body

  type pbody
end

module In = Dap.Request
module Out = Dap.Response
module Err = Dap.Response

module type Handler = sig
  include Types

  type in_msg

  type out_msg

  type t

  val make :
    handler:(in_msg In.t -> out_msg Out.t Dap_result.t) ->
    ctor:(out_msg -> out_msg Out.t) ->
    t

  val handle : t -> in_msg In.t -> out_msg Out.t Dap_result.t
end

module WithSeqr (T : Types) :
  Handler
    with type cmd := T.cmd
     and type args := T.args
     and type pargs := T.pargs
     and type body := T.body
     and type pbody := T.pbody
     and type in_msg = (T.cmd, T.args, T.pargs) In.Message.t
     and type out_msg = (T.cmd, T.body, T.pbody) Out.Message.t = struct
  (* NOTE the cmd param is the same for both the request and the response *)
  type in_msg = (T.cmd, T.args, T.pargs) In.Message.t

  type out_msg = (T.cmd, T.body, T.pbody) Out.Message.t

  type t = {
    handler : in_msg In.t -> out_msg Out.t Dap_result.t;
    ctor : out_msg -> out_msg Out.t;
  }

  let make ~handler ~ctor = {handler; ctor}

  let wrapper ~ctor f =
    let open Lwt_result in
    let getseq = In.(Fmap Message.seq) in
    let setseq seq request_seq =
      Out.(
        Fmap
          (fun msg ->
            msg |> Message.set_request_seq ~request_seq |> Message.set_seq ~seq))
    in
    let setseq_err seq request_seq =
      Err.(
        Fmap
          (fun msg ->
            msg |> Message.set_request_seq ~request_seq |> Message.set_seq ~seq))
    in
    fun msg ->
      let request_seq = In.(eval @@ Map (Val getseq, Val msg)) in
      let seq = 1 + request_seq in
      f msg
      |> Dap_result.to_lwt_result
      |> map (fun v -> Out.(
          ctor @@ eval @@ Map (Val (setseq seq request_seq), Val v)
        ))
      |> map_err (fun err -> Err.(
          errorResponse @@ eval @@ Map (Val (setseq_err seq request_seq), Val err)
        ))
      |> Dap_result.from_lwt_result

  let handle {handler; ctor} = wrapper ~ctor handler
end
