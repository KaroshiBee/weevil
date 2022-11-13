open Dap_types

(* LINK links Input and Output and ensures that the seqr numbers get set properly,
   the input message's seqr data is read and incremented then put onto both
   the output message's & state's seqr data *)
module LINK
    (IN_MSG_T : MSG_T)
    (IN_T : GADT_T)
    (OUT_MSG_T : MSG_T)
    (OUT_T : GADT_T) : sig
  module Make :
    functor
    (In : FULL_T(IN_MSG_T)(IN_T).T)
    (Out : FULL_T(OUT_MSG_T)(OUT_T).T)
    (S : STATE_T)
    -> LINK_T with type in_t := In.t and type out_t := Out.t and type state := S.t
end = struct
  module Make
      (In : FULL_T(IN_MSG_T)(IN_T).T)
      (Out : FULL_T(OUT_MSG_T)(OUT_T).T)
      (S : STATE_T) :
    LINK_T with type in_t := In.t and type out_t := Out.t and type state := S.t =
  struct
    type t = {
      handler : state:S.t -> In.t -> Out.t Dap_result.t;
    }

    let make ~handler =
      let wrapped_handler =
        let getseq = IN_T.(fmap_ IN_MSG_T.seq) in
        let setseq seq = OUT_T.(fmap_ (OUT_MSG_T.set_seq ~seq)) in
        let setseq_err seq = Err.(fmap_ (Message.set_seq ~seq)) in
        fun ~state msg ->
          let request_seq = IN_T.(eval @@ map_ (val_ getseq, val_ msg)) in
          let seq = 1 + request_seq in
          let s = Dap_base.Seqr.make ~seq ~request_seq () in
          (* setting the new seqr on state because one of the
             following two messages will always get sent back *)
          let () = S.set_seqr state s in
          handler state msg
          |> Dap_result.map ~f:(fun v ->
                 OUT_T.(Out.ctor @@ eval @@ map_ (val_ (setseq s), val_ v)))
          |> Dap_result.map_error ~f:(fun err ->
                 Err.(
                   errorResponse @@ eval @@ map_ (val_ (setseq_err s), val_ err)))
      in
      {handler = wrapped_handler}

    let string_to_input input =
      Result.(
        let r = Dap_js_msg.from_string input in
        bind r (fun msg ->
            try ok @@ Dap_js_msg.destruct In.enc msg with
            (* let wrong-encoder through *)
            | Dap_js_msg.Wrong_encoder _ as e -> raise e
            (* catch everything else and put into the result monad *)
            | _ as err -> error @@ Printexc.to_string err)
        |> Result.map In.ctor
        |> Result.map_error (fun err ->
               Err.(errorResponse @@ default_response_error err))
        |> Dap_result.from_result)

    let output_to_string output =
      OUT_T.(
        let to_string =
          fmap_ (fun msg ->
              let r =
                try Result.ok @@ Dap_js_msg.construct Out.enc msg
                with _ as err -> Result.error @@ Printexc.to_string err
              in
              Result.map Dap_js_msg.to_string r)
        in
        Lwt.return @@ eval @@ map_ (val_ to_string, val_ output))

    let handle t ~state msg =
      let v =
        string_to_input msg
        |> Dap_result.bind ~f:(t.handler ~state)
        |> Dap_result.to_lwt_error_as_str
      in
      (* turn into (string, string) Result.t and return either msg as the thing to send on to client *)
      Lwt_result.(v >>= output_to_string)
  end
end

(* LINK_RESTRICTED is a LINK that has just the one enum type for both In_msg and Out_msg ie Request/Response *)
module LINK_RESTRICTED
    (IN_MSG_T : MSG_T)
    (IN_T : GADT_T)
    (OUT_MSG_T : MSG_T)
    (OUT_T : GADT_T) : sig
  module Make : functor
    (In : FULL_T(IN_MSG_T)(IN_T).T)
    (Out : FULL_T(OUT_MSG_T)(OUT_T).T with type enum = In.enum)
    (S : STATE_T)
    -> LINK_T with type in_t := In.t and type out_t := Out.t and type state := S.t
end = struct
  module L = LINK (IN_MSG_T) (IN_T) (OUT_MSG_T) (OUT_T)

  module Make
      (In : FULL_T(IN_MSG_T)(IN_T).T)
      (Out : FULL_T(OUT_MSG_T)(OUT_T).T with type enum = In.enum)
      (S : STATE_T) :
    LINK_T with type in_t := In.t and type out_t := Out.t and type state := S.t =
    L.Make (In) (Out) (S)
end

(* raise an event or a response or whatever directly from the backend,
   NOTE will still have the correct seqr numbers, pulls last known values from state *)
module RAISE (OUT_MSG_T : MSG_T) (OUT_T : GADT_T) : sig
  module Make : functor (Out : FULL_T(OUT_MSG_T)(OUT_T).T) (S : STATE_T) ->
    LINK_T
      with type in_t := unit
       and type out_t := Out.t
       and type state := S.t
end = struct
  module Make (Out : FULL_T(OUT_MSG_T)(OUT_T).T) (S : STATE_T) :
    LINK_T
      with type in_t := unit
       and type out_t := Out.t
       and type state := S.t
= struct
    type t = {
      handler : state:S.t -> unit -> Out.t Dap_result.t;
    }

    let make ~handler =
      let wrapped_handler =
        let setseq seq = OUT_T.(fmap_ (OUT_MSG_T.set_seq ~seq)) in
        let setseq_err seq = Err.(fmap_ (Message.set_seq ~seq)) in
        fun ~state msg ->
          (* have to pull seqr data from state because we dont have an incoming message *)
          let seqr = S.current_seqr state in
          let request_seq = Dap_base.Seqr.seq seqr in
          let seq = 1 + request_seq in
          let s = Dap_base.Seqr.make ~seq ~request_seq () in
          (* setting the new seqr on state because one of the
             following two messages will always get sent back *)
          let () = S.set_seqr state s in
          handler state msg
          |> Dap_result.map ~f:(fun v ->
                 OUT_T.(Out.ctor @@ eval @@ map_ (val_ (setseq s), val_ v)))
          |> Dap_result.map_error ~f:(fun err ->
                 Err.(
                   errorResponse @@ eval @@ map_ (val_ (setseq_err s), val_ err)))
      in
      {handler = wrapped_handler}

    let string_to_input _input = Dap_result.ok ()

    let output_to_string output =
      OUT_T.(
        let to_string =
          fmap_ (fun msg ->
              let r =
                try Result.ok @@ Dap_js_msg.construct Out.enc msg
                with _ as err -> Result.error @@ Printexc.to_string err
              in
              Result.map Dap_js_msg.to_string r)
        in
        Lwt.return @@ eval @@ map_ (val_ to_string, val_ output))

    let handle t ~state msg =
      let v =
        string_to_input msg
        |> Dap_result.bind ~f:(t.handler ~state)
        |> Dap_result.to_lwt_error_as_str
      in
      (* turn into (string, string) Result.t and return either msg as the thing to send on to client *)
      Lwt_result.(v >>= output_to_string)
  end
end

module Request_response =
  LINK_RESTRICTED (Dap_request.Message) (Dap_request) (Dap_response.Message)
    (Dap_response)

module Raise_request = RAISE (Dap_request.Message) (Dap_request)

module Raise_response = RAISE (Dap_response.Message) (Dap_response)

module Raise_event = RAISE (Dap_event.Message) (Dap_event)
