module In = Dap.Response
module Out = Dap.Event
module Err = Dap.Response

module type Types = sig
  type cmd

  type body

  type pbody

  type ev

  type body_

  type pbody_

  type in_msg = (cmd, body, pbody) In.Message.t

  type out_msg = (ev, body_, pbody_) Out.Message.t

  val ctor_in : in_msg -> in_msg In.t

  val enc_in : in_msg Data_encoding.t

  val ctor_out : out_msg -> out_msg Out.t

  val enc_out : out_msg Data_encoding.t
end

module type T = sig
  include Types

  type t

  val make : handler:(in_msg In.t -> out_msg Out.t Dap_result.t) -> t

  val handle : t -> in_msg In.t -> out_msg Out.t Dap_result.t
end

module Make (Ty : Types) : T
with type cmd := Ty.cmd
 and type body := Ty.body
 and type pbody := Ty.pbody
 and type ev := Ty.ev
 and type body_ := Ty.body_
 and type pbody_ := Ty.pbody_
 and type in_msg := (Ty.cmd, Ty.body, Ty.pbody) In.Message.t
 and type out_msg := (Ty.ev, Ty.body_, Ty.pbody_) Out.Message.t = struct
  let ctor_in = Ty.ctor_in

  let enc_in = Ty.enc_in

  let ctor_out = Ty.ctor_out

  let enc_out = Ty.enc_out

  type t = {handler : Ty.in_msg In.t -> Ty.out_msg Out.t Dap_result.t}

  let make ~handler =
    let wrapped_handler =
      let getseq = In.(Fmap Message.seq) in
      let setseq seq = Out.(Fmap (Message.set_seq ~seq)) in
      let setseq_err seq request_seq =
        Err.(
          Fmap
            (fun msg ->
              msg
              |> Message.set_request_seq ~request_seq
              |> Message.set_seq ~seq))
      in
      fun msg ->
        let request_seq = In.(eval @@ Map (Val getseq, Val msg)) in
        let seq = 1 + request_seq in
        handler msg
        |> Dap_result.map ~f:(fun v ->
               Out.(Ty.ctor_out @@ eval @@ Map (Val (setseq seq), Val v)))
        |> Dap_result.map_error ~f:(fun err ->
               Err.(
                 errorResponse @@ eval
                 @@ Map (Val (setseq_err seq request_seq), Val err)))
    in
    {handler = wrapped_handler}

  let handle {handler} = handler
end
