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
  type in_msg

  type out_msg

  type t

  val make :
    handler:(Dap.Config.t -> in_msg In.t -> out_msg Out.t Dap_result.t) -> t

  val string_to_input : string -> in_msg In.t Dap_result.t

  val handle :
    t -> config:Dap.Config.t -> in_msg In.t -> out_msg Out.t Dap_result.t

  val output_to_string : out_msg Out.t -> (string, string) Lwt_result.t
end

module Make (Ty : Types) :
  T with type in_msg := Ty.in_msg and type out_msg := Ty.out_msg = struct
  type t = {
    handler :
      config:Dap.Config.t -> Ty.in_msg In.t -> Ty.out_msg Out.t Dap_result.t;
  }

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
      fun ~config msg ->
        let request_seq = In.(eval @@ Map (Val getseq, Val msg)) in
        let seq = 1 + request_seq in
        handler config msg
        |> Dap_result.map ~f:(fun v ->
               Out.(Ty.ctor_out @@ eval @@ Map (Val (setseq seq), Val v)))
        |> Dap_result.map_error ~f:(fun err ->
               Err.(
                 errorResponse @@ eval
                 @@ Map (Val (setseq_err seq request_seq), Val err)))
    in
    {handler = wrapped_handler}

  let string_to_input input =
    Result.(
      let r = Dap.Js_msg.from_string input in
      bind r (fun msg ->
          try ok @@ Dap.Js_msg.destruct Ty.enc_in msg with
          (* let wrong-encoder through *)
          | Dap.Js_msg.Wrong_encoder _ as e -> raise e
          (* catch everything else and put into the result monad *)
          | _ as err -> error @@ Printexc.to_string err)
      |> Result.map Ty.ctor_in
      |> Result.map_error (fun err ->
             Err.errorResponse @@ Dap.default_response_error err)
      |> Dap_result.from_result)

  let handle {handler} = handler

  let output_to_string output =
    Out.(
      let to_string =
        Fmap
          (fun msg ->
            let r =
              try Result.ok @@ Dap.Js_msg.construct Ty.enc_out msg
              with _ as err -> Result.error @@ Printexc.to_string err
            in
            Result.map Dap.Js_msg.to_string r)
      in
      Lwt.return @@ eval @@ Map (Val to_string, Val output))
end
