
module type SEQUENCED = sig
  type 'a t
  val seq : 'a t -> int
end

(* module Flow (Request:SEQUENCED) (Response:SEQUENCED) = struct *)

(*   module JS = Data_encoding.Json *)

(*   type ('request, 'response, 'event, 'error, 'cancel) t = { *)
(*     request: 'request Request.t Data_encoding.t; *)
(*     response: 'response Response.t Data_encoding.t; *)
(*     events: 'event list; *)
(*     on_error: (unit -> 'error) option; *)
(*     on_cancel: (unit -> 'cancel) option; *)
(*   } *)

(*   let make ?on_error ?on_cancel ?(events=[]) request response = { *)
(*       request; response; events; on_error; on_cancel *)
(*     } *)

(*   let _HEADER_FIELD = "Content-Length: " *)
(*   let _HEADER_TOKEN = "\r\n\r\n" *)

(*   let _replace input output = *)
(*     Str.global_replace (Str.regexp_string input) output *)

(*   let wrap_header js = *)
(*     let s = js *)
(*             |> JS.to_string *)
(*             |> _replace "\n" "" *)
(*     in *)
(*     let n = String.length s in *)
(*     Printf.sprintf "%s%d%s%s" _HEADER_FIELD n _HEADER_TOKEN s *)

(*   let destruct_request t msg = *)
(*       match JS.from_string msg with *)
(*       | Ok js -> ( *)
(*           try *)
(*             Ok (JS.destruct t.request js) *)
(*           with _ as err -> *)
(*             Logs.err (fun m -> m "Cannot parse json '%s' as request: '%s'" msg @@ Printexc.to_string err); *)
(*             Error (Printexc.to_string err) *)
(*         ) *)
(*       | Error err -> *)
(*         Logs.err (fun m -> m "Cannot parse json '%s': '%s'" msg err); *)
(*         (\* TODO should return an error response *\) *)
(*         Error err *)


(*   let construct_response t response = *)
(*     let r = Response.incr response in *)
(*     JS.construct t.response r |> wrap_header *)


(* end *)
(* open Dap_encoders *)

(* module Sequencing (S:SEQUENCED) : sig *)
(*   type t = {seq:int; request_seq:int} *)
(*   val correct_sequence : 'a S.t -> t *)
(* end *)
(*   = struct *)
(*   type t = {seq:int; request_seq:int} *)
(*   let correct_sequence req = *)
(*     let request_seq = S.seq req in *)
(*     let seq = succ request_seq in *)
(*     {seq; request_seq} *)
(* end *)

(* module type RequestResponse = sig *)
(*   type body *)
(*   type 'body t *)
(*   val of_request : request -> (body -> response, string) Result.t *)
(* end *)

(* module InitFlow : (RequestResponse with type body := Capabilities.t and type _ t = Capabilities.t InitializeResponseMessage.t) = struct *)
(*   type _ t = Capabilities.t InitializeResponseMessage.t *)
(*   let of_request = function *)
(*     | InitializeRequest req -> *)
(*       let module S = Sequencing (InitializeRequestMessage) in *)
(*       let {S.seq; request_seq} = S.correct_sequence req in *)
(*       let resp = fun body -> InitializeResponse (InitializeResponseMessage.make ~seq ~request_seq ~success:true ~body ()) in *)
(*       Result.Ok resp *)
(*     | _ -> *)
(*       Result.Error "Expected InitializeRequest" *)

(* end *)


(* type ('request, 'body, 'response) flow = 'request -> 'body -> ('response, ErrorResponse_body.t ErrorResponseMessage.t) Result.t *)
(* let cancel : (CancelArguments.t CancelRequestMessage.t, unit, unit CancelResponseMessage.t) flow *)
(*   = fun req body -> *)
(*     let module S = Sequencing (CancelRequestMessage) in *)
(*     let {S.seq; request_seq} = S.correct_sequence req in *)
(*     let resp = CancelResponseMessage.make ~seq ~request_seq ~success:true ~body () in *)
(*     Result.Ok resp *)

(* let init : (InitializeRequestArguments.t InitializeRequestMessage.t, Capabilities.t, Capabilities.t InitializeResponseMessage.t) flow *)
(*   = fun req body -> *)
(*     let module S = Sequencing (InitializeRequestMessage) in *)
(*     let {S.seq; request_seq} = S.correct_sequence req in *)
(*     let resp = InitializeResponseMessage.make ~seq ~request_seq ~success:true ~body () in *)
(*     Result.Ok resp *)

(* let config : (ConfigurationDoneArguments.t ConfigurationDoneRequestMessage.t, Dap_t.EmptyObject.t, Dap_t.EmptyObject.t ConfigurationDoneResponseMessage.t) flow *)
(*   = fun req body -> *)
(*     let module S = Sequencing (ConfigurationDoneRequestMessage) in *)
(*     let {S.seq; request_seq} = S.correct_sequence req in *)
(*     let resp = ConfigurationDoneResponseMessage.make ~seq ~request_seq ~success:true ~body () in *)
(*     Result.Ok resp *)

open Dap_t

module Flow (Request:REQUEST) (Response:RESPONSE) = struct

  open Dap_message

  type ('command, 'args, 'presence_args, 'body, 'presence_body) t = {
    request: ('command, 'args, 'presence_args) Request.t;
    response: ('command, 'body, 'presence_body) Response.t;
    events: event list;
    make_error: seq:int -> request_seq:int -> success:bool -> ?message:string -> body:ErrorResponse_body.t -> unit -> response ;
  }

  let make ?(events=[]) request response =
    let make_error = fun ~seq ~request_seq ~success ?message ~body () ->
      let cnl = Dap_command.cancel in
      let _b = EmptyObject.make () in
      let c = ResponseMessage.make_opt ~seq ~request_seq ~success ~command:cnl ?message ~body:_b () in
      let _ = CancelResponse c in
      let command = Dap_command.error in
      let e = ResponseMessage.make ~seq ~request_seq ~success ~command ?message ~body () in
      ErrorResponse e
    in
    {request; response; events; make_error}

end
