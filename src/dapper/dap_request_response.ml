module In = Dap.Request
module Out = Dap.Response
module Err = Dap.Response

module type Types = sig

  type cmd

  type args

  type pargs

  type body

  type pbody

  type in_msg = (cmd, args, pargs) In.Message.t

  type out_msg = (cmd, body, pbody) Out.Message.t

  val ctor_in : in_msg -> in_msg In.t
  val enc_in : in_msg Data_encoding.t

  val ctor_out : out_msg -> out_msg Out.t
  val enc_out : out_msg Data_encoding.t

end

(* NOTE the cmd param is the same for both the request and the response *)
module Make (T : Types) :
sig
  include Types
  type t
  val make : handler:(in_msg In.t -> out_msg Out.t Dap_result.t) -> t
  val handle : t -> in_msg In.t -> out_msg Out.t Dap_result.t
end
    with type cmd := T.cmd
     and type args := T.args
     and type pargs := T.pargs
     and type body := T.body
     and type pbody := T.pbody
     and type in_msg := T.in_msg
     and type out_msg := T.out_msg
= struct

  let ctor_in = T.ctor_in
  let enc_in = T.enc_in

  let ctor_out = T.ctor_out
  let enc_out = T.enc_out

  type t = {handler:T.in_msg In.t -> T.out_msg Out.t Dap_result.t}

  let make ~handler =
    let wrapped_handler =
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
        handler msg
        |> Dap_result.map ~f:(fun v -> Out.(
            T.ctor_out @@ eval @@ Map (Val (setseq seq request_seq), Val v)
          ))
        |> Dap_result.map_error ~f:(fun err -> Err.(
            errorResponse @@ eval @@ Map (Val (setseq_err seq request_seq), Val err)
          ))
    in
    {handler=wrapped_handler}

  let handle {handler} = handler

end
