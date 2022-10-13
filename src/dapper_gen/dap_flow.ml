
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
open Dap_encoders

module Sequencing (S:SEQUENCED) : sig
  type t = {seq:int; request_seq:int}
  val correct_sequence : 'a S.t -> t
end
  = struct
  type t = {seq:int; request_seq:int}
  let correct_sequence req =
    let request_seq = S.seq req in
    let seq = succ request_seq in
    {seq; request_seq}
end

module type RequestResponse = sig
  type body
  type 'body t
  val of_request : request -> (body -> response, string) Result.t
end

module InitFlow : (RequestResponse with type body := Capabilities.t and type _ t = Capabilities.t InitializeResponseMessage.t) = struct
  type _ t = Capabilities.t InitializeResponseMessage.t
  let of_request = function
    | InitializeRequest req ->
      let module S = Sequencing (InitializeRequestMessage) in
      let {S.seq; request_seq} = S.correct_sequence req in
      let resp = fun body -> InitializeResponse (InitializeResponseMessage.make ~seq ~request_seq ~success:true ~body ()) in
      Result.Ok resp
    | _ ->
      Result.Error "Expected InitializeRequest"

end


type ('request, 'body, 'response) flow = 'request -> 'body -> ('response, ErrorResponse_body.t ErrorResponseMessage.t) Result.t
let cancel : (CancelArguments.t CancelRequestMessage.t, unit, unit CancelResponseMessage.t) flow
  = fun req body ->
    let module S = Sequencing (CancelRequestMessage) in
    let {S.seq; request_seq} = S.correct_sequence req in
    let resp = CancelResponseMessage.make ~seq ~request_seq ~success:true ~body () in
    Result.Ok resp

let init : (InitializeRequestArguments.t InitializeRequestMessage.t, Capabilities.t, Capabilities.t InitializeResponseMessage.t) flow
  = fun req body ->
    let module S = Sequencing (InitializeRequestMessage) in
    let {S.seq; request_seq} = S.correct_sequence req in
    let resp = InitializeResponseMessage.make ~seq ~request_seq ~success:true ~body () in
    Result.Ok resp

let config : (ConfigurationDoneArguments.t ConfigurationDoneRequestMessage.t, Dap_t.EmptyObject.t, Dap_t.EmptyObject.t ConfigurationDoneResponseMessage.t) flow
  = fun req body ->
    let module S = Sequencing (ConfigurationDoneRequestMessage) in
    let {S.seq; request_seq} = S.correct_sequence req in
    let resp = ConfigurationDoneResponseMessage.make ~seq ~request_seq ~success:true ~body () in
    Result.Ok resp


 

(* let make_response = function *)
(*   | CancelRequest req -> *)
(*     let module S = Sequencing (CancelRequestMessage) in *)
(*     let {S.seq; request_seq} = S.correct_sequence req in *)
(*     let resp = CancelResponseMessage.make ~seq ~request_seq ~success:true in *)
(*     let aux : 'body -> response = fun body -> CancelResponse (resp ~body ()) in *)
(*     aux *)

(*   | InitializeRequest req -> *)
(*     let module S = Sequencing (InitializeRequestMessage) in *)
(*     let {S.seq; request_seq} = S.correct_sequence req in *)
(*     let resp = InitializeResponseMessage.make ~seq ~request_seq ~success:true in *)
(*     let aux : 'body -> response = fun body -> InitializeResponse (resp ~body ()) in *)
(*     aux *)

(*   | ConfigurationDoneRequest req -> *)
(*     let module S = Sequencing (ConfigurationDoneRequestMessage) in *)
(*     let {S.seq; request_seq} = S.correct_sequence req in *)
(*     let resp = ConfigurationDoneResponseMessage.make ~seq ~request_seq ~success:true in *)
(*     fun body:'body -> ConfigurationDoneResponse (resp ~body ()) *)

(*   | RunInTerminalRequest _ *)
(*   | LaunchRequest _ *)
(*   | AttachRequest _ *)
(*   | RestartRequest _ *)
(*   | DisconnectRequest _ *)
(*   | TerminateRequest _ *)
(*   | BreakpointLocationsRequest _ *)
(*   | SetBreakpointsRequest _ *)
(*   | SetFunctionBreakpointsRequest _ *)
(*   | SetExceptionBreakpointsRequest _ *)
(*   | DataBreakpointInfoRequest _ *)
(*   | SetDataBreakpointsRequest _ *)
(*   | SetInstructionBreakpointsRequest _ *)
(*   | ContinueRequest _ *)
(*   | NextRequest _ *)
(*   | StepInRequest _ *)
(*   | StepOutRequest _ *)
(*   | StepBackRequest _ *)
(*   | ReverseContinueRequest _ *)
(*   | RestartFrameRequest _ *)
(*   | GotoRequest _ *)
(*   | PauseRequest _ *)
(*   | StackTraceRequest _ *)
(*   | ScopesRequest _ *)
(*   | VariablesRequest _ *)
(*   | SetVariableRequest _ *)
(*   | SourceRequest _ *)
(*   | ThreadsRequest _ *)
(*   | TerminateThreadsRequest _ *)
(*   | ModulesRequest _ *)
(*   | LoadedSourcesRequest _ *)
(*   | EvaluateRequest _ *)
(*   | SetExpressionRequest _ *)
(*   | StepInTargetsRequest _ *)
(*   | GotoTargetsRequest _ *)
(*   | CompletionsRequest _ *)
(*   | ExceptionInfoRequest _ *)
(*   | ReadMemoryRequest _ *)
(*   | WriteMemoryRequest _ *)
(*   | DisassembleRequest _ *)
(*       -> assert false *)

(* module Flow = struct *)


(*   type ('request, 'response) t = { *)
(*     on_request: 'request -> 'response Lwt.t; *)
(*     response: 'response; *)
(*     events: event list; *)
(*     make_error: seq:int -> request_seq:int -> success:bool -> ?message:string -> body:ErrorResponse_body.t -> unit -> response ; *)
(*   } *)

(*   let make ?(events=[]) on_request response = *)
(*     let make_error = fun ~seq ~request_seq ~success ?message ~body () -> *)
(*       let e = ErrorResponseMessage.make ~seq ~request_seq ~success ?message ~body () in *)
(*       ErrorResponse e *)
(*     in *)
(*     {on_request; response; events; make_error} *)


(* end *)
