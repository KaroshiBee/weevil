open Dap_types

(* LINK links Input and Output and ensures that the seqr numbers get set properly,
   the input message's seq number is read and then put as the request seq for the response,
   also it is saved on the state for cases when we need to raise an error message *)
module LINK
    (IN_MSG_T : MSG_READONLY_T)
    (IN_T : GADT_T)
    (OUT_MSG_T : MSG_T)
    (OUT_T : GADT_T) : sig
  module Make :
    functor
    (In : FULL_READONLY_T(IN_MSG_T)(IN_T).T)
    (Out : FULL_T(OUT_MSG_T)(OUT_T).T)
    (S : STATE_T)
    -> LINK_T with type in_t := In.t and type out_t := Out.t and type state := S.t
end = struct
  module Make
      (In : FULL_READONLY_T(IN_MSG_T)(IN_T).T)
      (Out : FULL_T(OUT_MSG_T)(OUT_T).T)
      (S : STATE_T) :
    LINK_T with type in_t := In.t and type out_t := Out.t and type state := S.t =
  struct

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
          (fun msg ->
              let r =
                try Result.ok @@ Dap_js_msg.construct Out.enc msg
                with _ as err -> Result.error @@ Printexc.to_string err
              in
              Result.map Dap_js_msg.to_string r)
        in
        Lwt.return @@ eval @@ map_f ~f:to_string output)

    let make ~handler =
      let wrapped_handler =
        fun ~state in_t ->
          (* pull seq data from state and increment - seq numbers are a per-actor message id,
             ie the DAP has its own stream of seqs *)
          let seq = succ (S.current_seqr state |> Dap_base.Seqr.seq) in
          (* however we need the request seq number from the incoming message *)
          let args = IN_T.extract in_t in
          let request_seq = IN_MSG_T.seq args in
          let seqr = Dap_base.Seqr.make ~seq ~request_seq () in
          (* setting the new seqr on state because one of the
             following two messages will always get sent back,
             also NOTE we set the global state before invoking the handler *)
          let () = S.set_seqr state seqr in
          handler ~state in_t
          |> Dap_result.map ~f:(fun v ->
              let msg = OUT_T.extract v in
              let msg = OUT_MSG_T.set_seq ~seqr msg in
              Out.ctor msg
            )
          |> Dap_result.map_error ~f:(fun err ->
                let msg = Err.extract err in
                let msg = Err.Message.set_seq ~seqr msg in
                Err.errorResponse msg
            )
      in
      fun ~state msg ->
        let v =
          string_to_input msg
          |> Dap_result.bind ~f:(wrapped_handler ~state)
          |> Dap_result.to_lwt_error_as_str
        in
        (* turn into (string, string) Result.t and return either msg as the thing to send on to client *)
        Lwt_result.(v >>= output_to_string)
  end
end

(* LINK_RESTRICTED is a LINK that has just the one enum type for both In_msg and Out_msg ie Request/Response *)
module LINK_RESTRICTED
    (IN_MSG_T : MSG_READONLY_T)
    (IN_T : GADT_T)
    (OUT_MSG_T : MSG_T)
    (OUT_T : GADT_T) : sig
  module Make : functor
    (In : FULL_READONLY_T(IN_MSG_T)(IN_T).T)
    (Out : FULL_T(OUT_MSG_T)(OUT_T).T with type enum = In.enum)
    (S : STATE_T)
    -> LINK_T with type in_t := In.t and type out_t := Out.t and type state := S.t
end = struct
  module L = LINK (IN_MSG_T) (IN_T) (OUT_MSG_T) (OUT_T)

  module Make
      (In : FULL_READONLY_T(IN_MSG_T)(IN_T).T)
      (Out : FULL_T(OUT_MSG_T)(OUT_T).T with type enum = In.enum)
      (S : STATE_T) :
    LINK_T with type in_t := In.t and type out_t := Out.t and type state := S.t =
    L.Make (In) (Out) (S)
end

(* raise an event or a response or whatever directly from the backend,
   NOTE will pull the seqr number from last known values from state *)
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

    let string_to_input _input = Dap_result.ok ()

    let output_to_string output =
      OUT_T.(
        let to_string =
          (fun msg ->
              let r =
                try Result.ok @@ Dap_js_msg.construct Out.enc msg
                with _ as err -> Result.error @@ Printexc.to_string err
              in
              Result.map Dap_js_msg.to_string r)
        in
        Lwt.return @@ eval @@ map_f ~f:to_string output)

    let make ~handler =
      let wrapped_handler =
        fun ~state () ->
          (* pull seqr data from state *)
          let seqr = S.current_seqr state in
          (* always increment the seq number *)
          let seq = succ @@ Dap_base.Seqr.seq seqr in
          (* for this kind of RAISE we need to keep the request_seq as is,
             because these messages will all relate to an ongoing request in some way *)
          let request_seq = Dap_base.Seqr.request_seq seqr in
          (* make the new seqr pair *)
          let seqr = Dap_base.Seqr.make ~seq ~request_seq () in
          (* setting the new seqr on state because one of the
             following two messages will always get sent back
             NOTE we do this before calling the handler that's being wrapped *)
          let () = S.set_seqr state seqr in
          handler ~state ()
          |> Dap_result.map ~f:(fun v ->
              let msg = OUT_T.extract v in
              let msg = OUT_MSG_T.set_seq ~seqr msg in
              Out.ctor msg
            )
          |> Dap_result.map_error ~f:(fun err ->
                let msg = Err.extract err in
                let msg = Err.Message.set_seq ~seqr msg in
                Err.errorResponse msg
            )
      in
      fun ~state msg ->
        let v =
          string_to_input msg
          |> Dap_result.bind ~f:(wrapped_handler ~state)
          |> Dap_result.to_lwt_error_as_str
        in
        (* turn into (string, string) Result.t and return either msg as the thing to send on to client *)
        Lwt_result.(v >>= output_to_string)
  end
end

(* raise an error response directly from the backend,
   NOTE will pull the seqr number from last known values from state *)
module RAISE_ERR (OUT_MSG_T : MSG_T) (OUT_T : GADT_T) : sig
  module Make : functor (Out : FULL_T(OUT_MSG_T)(OUT_T).T) (S : STATE_T) ->
    LINK_T
      with type in_t := string
       and type out_t := Out.t
       and type state := S.t
end = struct
  module Make (Out : FULL_T(OUT_MSG_T)(OUT_T).T) (S : STATE_T) :
    LINK_T
      with type in_t := string
       and type out_t := Out.t
       and type state := S.t
= struct

    let string_to_input input = Dap_result.ok input

    let output_to_string output =
      OUT_T.(
        let to_string =
          (fun msg ->
              let r =
                try Result.ok @@ Dap_js_msg.construct Out.enc msg
                with _ as err -> Result.error @@ Printexc.to_string err
              in
              Result.map Dap_js_msg.to_string r)
        in
        Lwt.return @@ eval @@ map_f ~f:to_string output)

    let make ~handler =
      let wrapped_handler =
        fun ~state err_str ->
          (* have to pull seqr data from state because we dont have an incoming message *)
          let seqr = S.current_seqr state in
          (* always increment the seq number *)
          let seq = succ @@ Dap_base.Seqr.seq seqr in
          (* we also need to inc the last request_seq as we will have
             raised an error in response to a request that we cannot handle, *)
          let request_seq = succ @@ Dap_base.Seqr.request_seq seqr in
          (* make the new seqr pair *)
          let seqr = Dap_base.Seqr.make ~seq ~request_seq () in
          (* setting the new seqr on state because one of the
             following two messages will always get sent back
             NOTE we do this before calling the handler that's being wrapped *)
          let () = S.set_seqr state seqr in
          handler ~state err_str
            |> Dap_result.map ~f:(fun v ->
              let msg = OUT_T.extract v in
              let msg = OUT_MSG_T.set_seq ~seqr msg in
              Out.ctor msg
            )
          |> Dap_result.map_error ~f:(fun err ->
                let msg = Err.extract err in
                let msg = Err.Message.set_seq ~seqr msg in
                Err.errorResponse msg
            )
      in
      fun ~state msg ->
        let v =
          string_to_input msg
          |> Dap_result.bind ~f:(wrapped_handler ~state)
          |> Dap_result.to_lwt_error_as_str
        in
        (* turn into (string, string) Result.t and return either msg as the thing to send on to client *)
        Lwt_result.(v >>= output_to_string)
  end
end

(* NOTE that we use the read-only version of Dap_request msg here *)
module Request_response =
  LINK_RESTRICTED (Dap_request.Message) (Dap_request) (Dap_response.Message) (Dap_response)

(* NOTE events can just get raised - sequencing is taken from last known request seq# *)
module Raise_event = RAISE (Dap_event.Message) (Dap_event)

(* NOTE special error raising for when we get messages from front end
   that we cannot deal with at all *)
module Raise_error = RAISE_ERR (Dap_response.Message) (Dap_response)



(* (\* NOTE that we use the read-write version of Dap_request msg here *)
(*    this is for the 'reverse-request' messages that the DAP protocol specifies, *)
(*    Not sure we will need to support this functionality *\) *)
(* module Raise_request = RAISE (Dap_request_message) (Dap_request) *)
(* module Raise_response = RAISE (Dap_response.Message) (Dap_response) *)
