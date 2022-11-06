(* module In = Dap.Response *)
(* module Out = Dap.Event *)
module Err = Dap.Response

module type Msg_T = sig

  type ('enum, 'contents, 'presence) t

  val seq : (_, _, _) t -> int

  val set_seq : seq:Dap_base.Seqr.t -> ('a,'b,'c) t -> ('a,'b,'c) t
end

module type T_ = sig

  type 'a t

  type 'a expr

  val fmap_ : ('a -> 'b) -> ('a -> 'b) t

  val val_ : 'msg t -> 'msg expr

  val map_ : ('msg -> 'b) expr * 'msg expr -> 'b expr

  val eval : 'msg. 'msg expr -> 'msg

end

module type Types = sig

  type cmd

  type body

  type pbody

  type ev

  type body_

  type pbody_

  module In_msg : Msg_T
  module In : T_
  type in_msg = (cmd, body, pbody) In_msg.t

  module Out_msg : Msg_T
  module Out : T_
  type out_msg = (ev, body_, pbody_) Out_msg.t

  val ctor_in : in_msg -> in_msg In.t

  val enc_in : in_msg Data_encoding.t

  val ctor_out : out_msg -> out_msg Out.t

  val enc_out : out_msg Data_encoding.t

end

module type T = sig
  module In_msg : Msg_T
  module In : T_
  type in_msg

  module Out_msg : Msg_T
  module Out : T_
  type out_msg

  type 'in_msg in_t

  type 'out_msg out_t

  type t

  val make : t

  val string_to_input : string -> in_msg in_t Dap_result.t

  val handle :
    t -> config:Dap.Config.t -> in_msg in_t -> out_msg out_t Dap_result.t

  val output_to_string : out_msg out_t -> (string, string) Lwt_result.t
end

module Make
    (Ty : Types) :
  T with type in_msg := Ty.in_msg
     and type out_msg := Ty.out_msg
= struct

  module In_msg = Ty.In_msg
  module In = Ty.In
  type 'a in_t = 'a In.t

  module Out_msg = Ty.Out_msg
  module Out = Ty.Out
  type 'a out_t = 'a Out.t


  type t = {
    handler :
      config:Dap.Config.t ->
      Ty.in_msg In.t ->
      Ty.out_msg Out.t Dap_result.t;
  }

  let make ~handler =
    let wrapped_handler =
      let getseq = In.(fmap_ In_msg.seq) in
      let setseq seq = Out.(fmap_ (Out_msg.set_seq ~seq)) in
      let setseq_err seq = Err.(fmap_ (Message.set_seq ~seq)) in
      fun ~config msg ->
        let request_seq = In.(eval @@ map_ (val_ getseq, val_ msg)) in
        let seq = 1 + request_seq in
        let s = Dap_base.Seqr.make ~seq ~request_seq () in
        handler config msg
        |> Dap_result.map ~f:(fun v ->
               Out.(Ty.ctor_out @@ eval @@ map_ (val_ (setseq s), val_ v)))
        |> Dap_result.map_error ~f:(fun err ->
               Err.(
                 errorResponse @@ eval
                 @@ map_ (val_ (setseq_err s), val_ err)))
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
        fmap_
          (fun msg ->
            let r =
              try Result.ok @@ Dap.Js_msg.construct Ty.enc_out msg
              with _ as err -> Result.error @@ Printexc.to_string err
            in
            Result.map Dap.Js_msg.to_string r)
      in
      Lwt.return @@ eval @@ map_ (val_ to_string, val_ output))
end
