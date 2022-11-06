module Err = Dap.Response

module type STATE_T = sig
  type t

  val make_empty : t

  val process_none : t -> Lwt_process.process_none option

  val set_process_none : t -> Lwt_process.process_none -> unit

  val ic : t -> Lwt_io.input_channel option

  val oc : t -> Lwt_io.output_channel option

  val set_io : t -> Lwt_io.input_channel -> Lwt_io.output_channel -> unit

  val launch_mode : t -> Dap_base.launch_mode option

  val set_launch_mode : t -> Dap_base.launch_mode -> unit
end

module type MSG_T = sig
  type ('enum, 'contents, 'presence) t

  val seq : (_, _, _) t -> int

  val set_seq :
    seq:Dap_base.Seqr.t ->
    ('enum, 'contents, 'presence) t ->
    ('enum, 'contents, 'presence) t
end

module type GADT_T = sig
  type 'a t

  type 'a expr

  val fmap_ : ('a -> 'b) -> ('a -> 'b) t

  val val_ : 'msg t -> 'msg expr

  val map_ : ('msg -> 'b) expr * 'msg expr -> 'b expr

  val eval : 'msg. 'msg expr -> 'msg
end

module Types (In_msg : MSG_T) (In : GADT_T) (Out_msg : MSG_T) (Out : GADT_T) =
struct
  module type T1 = sig
    type enum

    type in_contents

    type in_presence

    type out_contents

    type out_presence

    type in_msg = (enum, in_contents, in_presence) In_msg.t

    type out_msg = (enum, out_contents, out_presence) Out_msg.t

    val ctor_in : in_msg -> in_msg In.t

    val enc_in : in_msg Data_encoding.t

    val ctor_out : out_msg -> out_msg Out.t

    val enc_out : out_msg Data_encoding.t
  end

  module type T2 = sig
    type in_enum

    type in_contents

    type in_presence

    type out_enum

    type out_contents

    type out_presence

    type in_msg = (in_enum, in_contents, in_presence) In_msg.t

    type out_msg = (out_enum, out_contents, out_presence) Out_msg.t

    val ctor_in : in_msg -> in_msg In.t

    val enc_in : in_msg Data_encoding.t

    val ctor_out : out_msg -> out_msg Out.t

    val enc_out : out_msg Data_encoding.t
  end
end

module type HANDLER = sig
  type in_t

  type out_t

  type t

  val make : handler:(Dap.Config.t -> in_t -> out_t Dap_result.t) -> t

  val string_to_input : string -> in_t Dap_result.t

  val handle : t -> config:Dap.Config.t -> in_t -> out_t Dap_result.t

  val output_to_string : out_t -> (string, string) Lwt_result.t
end

(* Maker1 has just the one enum type for both In_msg and Out_msg ie Request/Response *)
module Maker1 (In_msg : MSG_T) (In : GADT_T) (Out_msg : MSG_T) (Out : GADT_T) =
struct
  module TY = Types (In_msg) (In) (Out_msg) (Out)

  module Make (Ty : TY.T1) :
    HANDLER with type in_t := Ty.in_msg In.t and type out_t := Ty.out_msg Out.t =
  struct
    type t = {
      handler :
        config:Dap.Config.t -> Ty.in_msg In.t -> Ty.out_msg Out.t Dap_result.t;
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
                   errorResponse @@ eval @@ map_ (val_ (setseq_err s), val_ err)))
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
          fmap_ (fun msg ->
              let r =
                try Result.ok @@ Dap.Js_msg.construct Ty.enc_out msg
                with _ as err -> Result.error @@ Printexc.to_string err
              in
              Result.map Dap.Js_msg.to_string r)
        in
        Lwt.return @@ eval @@ map_ (val_ to_string, val_ output))
  end
end

(* Maker2 has two enums, an in_enum and an out_enum *)
module Maker2 (In_msg : MSG_T) (In : GADT_T) (Out_msg : MSG_T) (Out : GADT_T) =
struct
  module TY = Types (In_msg) (In) (Out_msg) (Out)

  module Make (Ty : TY.T2) :
    HANDLER with type in_t := Ty.in_msg In.t and type out_t := Ty.out_msg Out.t =
  struct
    type t = {
      handler :
        config:Dap.Config.t -> Ty.in_msg In.t -> Ty.out_msg Out.t Dap_result.t;
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
                   errorResponse @@ eval @@ map_ (val_ (setseq_err s), val_ err)))
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
          fmap_ (fun msg ->
              let r =
                try Result.ok @@ Dap.Js_msg.construct Ty.enc_out msg
                with _ as err -> Result.error @@ Printexc.to_string err
              in
              Result.map Dap.Js_msg.to_string r)
        in
        Lwt.return @@ eval @@ map_ (val_ to_string, val_ output))
  end
end

module Request_response =
  Maker1 (Dap.Request.Message) (Dap.Request) (Dap.Response.Message) (Dap.Response)
module Response_event =
  Maker2 (Dap.Response.Message) (Dap.Response) (Dap.Event.Message) (Dap.Event)
module Raise_event =
  Maker2 (Dap.Event.Message) (Dap.Event) (Dap.Event.Message) (Dap.Event)
