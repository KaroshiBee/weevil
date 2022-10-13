open Dap_render

let%expect_test "Check ErrorResponse example" =
  let schema_js = Ezjsonm.from_channel @@ open_in "data/errorResponse.json" in
  let dfs = Dfs.make ~schema_js in
  let actual = render dfs in
  Printf.printf "%s" actual;
  [%expect {|
    (* NOTE this file was autogenerated - do not modify by hand *)

    open Dap_t

    module Command = struct
    type t = Error

    let enc =
     let open Data_encoding in
     conv
     (function Error -> "error")
     (function "error" -> Error | _ -> failwith "Command")
     string

    end


    module Event = struct
    type t =

    let enc =
     let open Data_encoding in
     conv
     (function )
     (function _ -> failwith "Event")
     string

    end


    module Message = struct
    type t = { id: int;
    format: string;
    variables: Data_encoding.json option;
    sendTelemetry: bool option;
    showUser: bool option;
    url: string option;
    urlLabel: string option;
    lines: int list option; }

    let enc =
     let open Data_encoding in
     (* Message.t *)
             conv
     (fun {id; format; variables; sendTelemetry; showUser; url; urlLabel; lines} -> (id, format, variables, sendTelemetry, showUser, url, urlLabel, lines))
     (fun (id, format, variables, sendTelemetry, showUser, url, urlLabel, lines) -> {id; format; variables; sendTelemetry; showUser; url; urlLabel; lines})
     (obj8
    (req "id" int31)
    (req "format" string)
    (opt "variables" json)
    (opt "sendTelemetry" bool)
    (opt "showUser" bool)
    (opt "url" string)
    (opt "urlLabel" string)
    (opt "lines" (list int31)))


    let make ~id ~format ?variables ?sendTelemetry ?showUser ?url ?urlLabel ?lines () =
    {id; format; variables; sendTelemetry; showUser; url; urlLabel; lines}

    end


    module ErrorResponse_body = struct
    type t = { error: Message.t option; }

    let enc =
     let open Data_encoding in
     (* ErrorResponse_body.t *)
             conv
     (fun {error} -> error)
     (fun error -> {error})
     (obj1
    (opt "error" Message.enc))


    let make ?error () =
    {error}

    end


    module ErrorResponseMessage = MakeResponse (struct type t = Command.t let value=Command.Error let enc = Command.enc end)

    type request =


    type response =
    | ErrorResponse of ErrorResponse_body.t ErrorResponseMessage.t

    type event = |}]


let%expect_test "Check CancelRequest example" =
  let schema_js = Ezjsonm.from_channel @@ open_in "data/cancelRequest.json" in
  let dfs = Dfs.make ~schema_js in
  let actual = render dfs in
  Printf.printf "%s" actual;
  [%expect {|
    (* NOTE this file was autogenerated - do not modify by hand *)

    open Dap_t

    module Command = struct
    type t = Cancel | Error

    let enc =
     let open Data_encoding in
     conv
     (function Cancel -> "cancel" | Error -> "error")
     (function "cancel" -> Cancel | "error" -> Error | _ -> failwith "Command")
     string

    end


    module Event = struct
    type t =

    let enc =
     let open Data_encoding in
     conv
     (function )
     (function _ -> failwith "Event")
     string

    end


    module CancelArguments = struct
    type t = { requestId: int option;
    progressId: string option; }

    let enc =
     let open Data_encoding in
     (* CancelArguments.t *)
             conv
     (fun {requestId; progressId} -> (requestId, progressId))
     (fun (requestId, progressId) -> {requestId; progressId})
     (obj2
    (opt "requestId" int31)
    (opt "progressId" string))


    let make ?requestId ?progressId () =
    {requestId; progressId}

    end


    module CancelRequestMessage = MakeRequest_optionalArgs (struct type t = Command.t let value=Command.Cancel let enc = Command.enc end)

    type request =
    | CancelRequest of CancelArguments.t CancelRequestMessage.t

    type response =


    type event = |}]


let%expect_test "Check StoppedEvent example" =
  let schema_js = Ezjsonm.from_channel @@ open_in "data/stoppedEvent.json" in
  let dfs = Dfs.make ~schema_js in
  let actual = render dfs in
  Printf.printf "%s" actual;
  [%expect {|
    (* NOTE this file was autogenerated - do not modify by hand *)

    open Dap_t

    module Command = struct
    type t = Error

    let enc =
     let open Data_encoding in
     conv
     (function Error -> "error")
     (function "error" -> Error | _ -> failwith "Command")
     string

    end


    module Event = struct
    type t = Stopped

    let enc =
     let open Data_encoding in
     conv
     (function Stopped -> "stopped")
     (function "stopped" -> Stopped | _ -> failwith "Event")
     string

    end


    module StoppedEvent_body_reason = struct
    type t = Step | Breakpoint | Exception | Pause | Entry | Goto | Function_breakpoint | Data_breakpoint | Instruction_breakpoint | Other of string

    let enc =
     let open Data_encoding in
     conv
     (function Step -> "step" | Breakpoint -> "breakpoint" | Exception -> "exception" | Pause -> "pause" | Entry -> "entry" | Goto -> "goto" | Function_breakpoint -> "function breakpoint" | Data_breakpoint -> "data breakpoint" | Instruction_breakpoint -> "instruction breakpoint" | Other s -> s)
     (function "step" -> Step | "breakpoint" -> Breakpoint | "exception" -> Exception | "pause" -> Pause | "entry" -> Entry | "goto" -> Goto | "function breakpoint" -> Function_breakpoint | "data breakpoint" -> Data_breakpoint | "instruction breakpoint" -> Instruction_breakpoint | _ as s -> Other s)
     string

    end


    module StoppedEvent_body = struct
    type t = { reason: StoppedEvent_body_reason.t;
    description: string option;
    threadId: int option;
    preserveFocusHint: bool option;
    text: string option;
    allThreadsStopped: bool option;
    hitBreakpointIds: int list option; }

    let enc =
     let open Data_encoding in
     (* StoppedEvent_body.t *)
             conv
     (fun {reason; description; threadId; preserveFocusHint; text; allThreadsStopped; hitBreakpointIds} -> (reason, description, threadId, preserveFocusHint, text, allThreadsStopped, hitBreakpointIds))
     (fun (reason, description, threadId, preserveFocusHint, text, allThreadsStopped, hitBreakpointIds) -> {reason; description; threadId; preserveFocusHint; text; allThreadsStopped; hitBreakpointIds})
     (obj7
    (req "reason" StoppedEvent_body_reason.enc)
    (opt "description" string)
    (opt "threadId" int31)
    (opt "preserveFocusHint" bool)
    (opt "text" string)
    (opt "allThreadsStopped" bool)
    (opt "hitBreakpointIds" (list int31)))


    let make ~reason ?description ?threadId ?preserveFocusHint ?text ?allThreadsStopped ?hitBreakpointIds () =
    {reason; description; threadId; preserveFocusHint; text; allThreadsStopped; hitBreakpointIds}

    end


    module StoppedEventMessage = MakeEvent (struct type t = Event.t let value=Event.Stopped let enc = Event.enc end)

    type request =


    type response =


    type event =
    | StoppedEvent of StoppedEvent_body.t StoppedEventMessage.t |}]

let%expect_test "Check cyclic example" =
  let schema_js = Ezjsonm.from_channel @@ open_in "data/cyclic.json" in
  let dfs = Dfs.make ~schema_js in
  let actual = render dfs in
  Printf.printf "%s" actual;
  [%expect {|
    (* NOTE this file was autogenerated - do not modify by hand *)

    open Dap_t

    module Command = struct
    type t = Error

    let enc =
     let open Data_encoding in
     conv
     (function Error -> "error")
     (function "error" -> Error | _ -> failwith "Command")
     string

    end


    module Event = struct
    type t =

    let enc =
     let open Data_encoding in
     conv
     (function )
     (function _ -> failwith "Event")
     string

    end


    module ExceptionDetails = struct
    type t = { message: string option;
    typeName: string option;
    fullTypeName: string option;
    evaluateName: string option;
    stackTrace: string option;
    innerException: t list option; }

    let enc =
     let open Data_encoding in
     mu "ExceptionDetails.t"
     ( fun e ->
     conv
     (fun {message; typeName; fullTypeName; evaluateName; stackTrace; innerException} -> (message, typeName, fullTypeName, evaluateName, stackTrace, innerException))
     (fun (message, typeName, fullTypeName, evaluateName, stackTrace, innerException) -> {message; typeName; fullTypeName; evaluateName; stackTrace; innerException})
     (obj6
    (opt "message" string)
    (opt "typeName" string)
    (opt "fullTypeName" string)
    (opt "evaluateName" string)
    (opt "stackTrace" string)
    (opt "innerException" (list e)))
    )

    let make ?message ?typeName ?fullTypeName ?evaluateName ?stackTrace ?innerException () =
    {message; typeName; fullTypeName; evaluateName; stackTrace; innerException}

    end


    type request =


    type response =


    type event = |}]

let%expect_test "Check large example" =
  let schema_js = Ezjsonm.from_channel @@ open_in "data/largeObject.json" in
  let dfs = Dfs.make ~schema_js in
  let actual = render dfs in
  Printf.printf "%s" actual;
  [%expect {|
    (* NOTE this file was autogenerated - do not modify by hand *)

    open Dap_t

    module Command = struct
    type t = Error

    let enc =
     let open Data_encoding in
     conv
     (function Error -> "error")
     (function "error" -> Error | _ -> failwith "Command")
     string

    end


    module Event = struct
    type t =

    let enc =
     let open Data_encoding in
     conv
     (function )
     (function _ -> failwith "Event")
     string

    end


    module Capabilities = struct
    module Capabilities_0 = struct
    type t = { supportsConfigurationDoneRequest: bool option;
    supportsFunctionBreakpoints: bool option;
    supportsConditionalBreakpoints: bool option;
    supportsHitConditionalBreakpoints: bool option;
    supportsEvaluateForHovers: bool option;
    supportsStepBack: bool option;
    supportsSetVariable: bool option;
    supportsRestartFrame: bool option;
    supportsGotoTargetsRequest: bool option;
    supportsStepInTargetsRequest: bool option; }

    let enc =
     let open Data_encoding in
     (* Capabilities_0.t *)
             conv
     (fun {supportsConfigurationDoneRequest; supportsFunctionBreakpoints; supportsConditionalBreakpoints; supportsHitConditionalBreakpoints; supportsEvaluateForHovers; supportsStepBack; supportsSetVariable; supportsRestartFrame; supportsGotoTargetsRequest; supportsStepInTargetsRequest} -> (supportsConfigurationDoneRequest, supportsFunctionBreakpoints, supportsConditionalBreakpoints, supportsHitConditionalBreakpoints, supportsEvaluateForHovers, supportsStepBack, supportsSetVariable, supportsRestartFrame, supportsGotoTargetsRequest, supportsStepInTargetsRequest))
     (fun (supportsConfigurationDoneRequest, supportsFunctionBreakpoints, supportsConditionalBreakpoints, supportsHitConditionalBreakpoints, supportsEvaluateForHovers, supportsStepBack, supportsSetVariable, supportsRestartFrame, supportsGotoTargetsRequest, supportsStepInTargetsRequest) -> {supportsConfigurationDoneRequest; supportsFunctionBreakpoints; supportsConditionalBreakpoints; supportsHitConditionalBreakpoints; supportsEvaluateForHovers; supportsStepBack; supportsSetVariable; supportsRestartFrame; supportsGotoTargetsRequest; supportsStepInTargetsRequest})
     (obj10
    (opt "supportsConfigurationDoneRequest" bool)
    (opt "supportsFunctionBreakpoints" bool)
    (opt "supportsConditionalBreakpoints" bool)
    (opt "supportsHitConditionalBreakpoints" bool)
    (opt "supportsEvaluateForHovers" bool)
    (opt "supportsStepBack" bool)
    (opt "supportsSetVariable" bool)
    (opt "supportsRestartFrame" bool)
    (opt "supportsGotoTargetsRequest" bool)
    (opt "supportsStepInTargetsRequest" bool))


    let make ?supportsConfigurationDoneRequest ?supportsFunctionBreakpoints ?supportsConditionalBreakpoints ?supportsHitConditionalBreakpoints ?supportsEvaluateForHovers ?supportsStepBack ?supportsSetVariable ?supportsRestartFrame ?supportsGotoTargetsRequest ?supportsStepInTargetsRequest () =
    {supportsConfigurationDoneRequest; supportsFunctionBreakpoints; supportsConditionalBreakpoints; supportsHitConditionalBreakpoints; supportsEvaluateForHovers; supportsStepBack; supportsSetVariable; supportsRestartFrame; supportsGotoTargetsRequest; supportsStepInTargetsRequest}

    end


    module Capabilities_10 = struct
    type t = { supportsCompletionsRequest: bool option;
    completionTriggerCharacters: string list option;
    supportsModulesRequest: bool option;
    supportsRestartRequest: bool option;
    supportsExceptionOptions: bool option;
    supportsValueFormattingOptions: bool option;
    supportsExceptionInfoRequest: bool option;
    supportTerminateDebuggee: bool option;
    supportSuspendDebuggee: bool option;
    supportsDelayedStackTraceLoading: bool option; }

    let enc =
     let open Data_encoding in
     (* Capabilities_10.t *)
             conv
     (fun {supportsCompletionsRequest; completionTriggerCharacters; supportsModulesRequest; supportsRestartRequest; supportsExceptionOptions; supportsValueFormattingOptions; supportsExceptionInfoRequest; supportTerminateDebuggee; supportSuspendDebuggee; supportsDelayedStackTraceLoading} -> (supportsCompletionsRequest, completionTriggerCharacters, supportsModulesRequest, supportsRestartRequest, supportsExceptionOptions, supportsValueFormattingOptions, supportsExceptionInfoRequest, supportTerminateDebuggee, supportSuspendDebuggee, supportsDelayedStackTraceLoading))
     (fun (supportsCompletionsRequest, completionTriggerCharacters, supportsModulesRequest, supportsRestartRequest, supportsExceptionOptions, supportsValueFormattingOptions, supportsExceptionInfoRequest, supportTerminateDebuggee, supportSuspendDebuggee, supportsDelayedStackTraceLoading) -> {supportsCompletionsRequest; completionTriggerCharacters; supportsModulesRequest; supportsRestartRequest; supportsExceptionOptions; supportsValueFormattingOptions; supportsExceptionInfoRequest; supportTerminateDebuggee; supportSuspendDebuggee; supportsDelayedStackTraceLoading})
     (obj10
    (opt "supportsCompletionsRequest" bool)
    (opt "completionTriggerCharacters" (list string))
    (opt "supportsModulesRequest" bool)
    (opt "supportsRestartRequest" bool)
    (opt "supportsExceptionOptions" bool)
    (opt "supportsValueFormattingOptions" bool)
    (opt "supportsExceptionInfoRequest" bool)
    (opt "supportTerminateDebuggee" bool)
    (opt "supportSuspendDebuggee" bool)
    (opt "supportsDelayedStackTraceLoading" bool))


    let make ?supportsCompletionsRequest ?completionTriggerCharacters ?supportsModulesRequest ?supportsRestartRequest ?supportsExceptionOptions ?supportsValueFormattingOptions ?supportsExceptionInfoRequest ?supportTerminateDebuggee ?supportSuspendDebuggee ?supportsDelayedStackTraceLoading () =
    {supportsCompletionsRequest; completionTriggerCharacters; supportsModulesRequest; supportsRestartRequest; supportsExceptionOptions; supportsValueFormattingOptions; supportsExceptionInfoRequest; supportTerminateDebuggee; supportSuspendDebuggee; supportsDelayedStackTraceLoading}

    end


    module Capabilities_20 = struct
    type t = { supportsLoadedSourcesRequest: bool option;
    supportsLogPoints: bool option;
    supportsTerminateThreadsRequest: bool option;
    supportsSetExpression: bool option;
    supportsTerminateRequest: bool option;
    supportsDataBreakpoints: bool option;
    supportsReadMemoryRequest: bool option;
    supportsWriteMemoryRequest: bool option;
    supportsDisassembleRequest: bool option;
    supportsCancelRequest: bool option; }

    let enc =
     let open Data_encoding in
     (* Capabilities_20.t *)
             conv
     (fun {supportsLoadedSourcesRequest; supportsLogPoints; supportsTerminateThreadsRequest; supportsSetExpression; supportsTerminateRequest; supportsDataBreakpoints; supportsReadMemoryRequest; supportsWriteMemoryRequest; supportsDisassembleRequest; supportsCancelRequest} -> (supportsLoadedSourcesRequest, supportsLogPoints, supportsTerminateThreadsRequest, supportsSetExpression, supportsTerminateRequest, supportsDataBreakpoints, supportsReadMemoryRequest, supportsWriteMemoryRequest, supportsDisassembleRequest, supportsCancelRequest))
     (fun (supportsLoadedSourcesRequest, supportsLogPoints, supportsTerminateThreadsRequest, supportsSetExpression, supportsTerminateRequest, supportsDataBreakpoints, supportsReadMemoryRequest, supportsWriteMemoryRequest, supportsDisassembleRequest, supportsCancelRequest) -> {supportsLoadedSourcesRequest; supportsLogPoints; supportsTerminateThreadsRequest; supportsSetExpression; supportsTerminateRequest; supportsDataBreakpoints; supportsReadMemoryRequest; supportsWriteMemoryRequest; supportsDisassembleRequest; supportsCancelRequest})
     (obj10
    (opt "supportsLoadedSourcesRequest" bool)
    (opt "supportsLogPoints" bool)
    (opt "supportsTerminateThreadsRequest" bool)
    (opt "supportsSetExpression" bool)
    (opt "supportsTerminateRequest" bool)
    (opt "supportsDataBreakpoints" bool)
    (opt "supportsReadMemoryRequest" bool)
    (opt "supportsWriteMemoryRequest" bool)
    (opt "supportsDisassembleRequest" bool)
    (opt "supportsCancelRequest" bool))


    let make ?supportsLoadedSourcesRequest ?supportsLogPoints ?supportsTerminateThreadsRequest ?supportsSetExpression ?supportsTerminateRequest ?supportsDataBreakpoints ?supportsReadMemoryRequest ?supportsWriteMemoryRequest ?supportsDisassembleRequest ?supportsCancelRequest () =
    {supportsLoadedSourcesRequest; supportsLogPoints; supportsTerminateThreadsRequest; supportsSetExpression; supportsTerminateRequest; supportsDataBreakpoints; supportsReadMemoryRequest; supportsWriteMemoryRequest; supportsDisassembleRequest; supportsCancelRequest}

    end


    module Capabilities_30 = struct
    type t = { supportsBreakpointLocationsRequest: bool option;
    supportsClipboardContext: bool option;
    supportsSteppingGranularity: bool option;
    supportsInstructionBreakpoints: bool option;
    supportsExceptionFilterOptions: bool option;
    supportsSingleThreadExecutionRequests: bool option; }

    let enc =
     let open Data_encoding in
     (* Capabilities_30.t *)
             conv
     (fun {supportsBreakpointLocationsRequest; supportsClipboardContext; supportsSteppingGranularity; supportsInstructionBreakpoints; supportsExceptionFilterOptions; supportsSingleThreadExecutionRequests} -> (supportsBreakpointLocationsRequest, supportsClipboardContext, supportsSteppingGranularity, supportsInstructionBreakpoints, supportsExceptionFilterOptions, supportsSingleThreadExecutionRequests))
     (fun (supportsBreakpointLocationsRequest, supportsClipboardContext, supportsSteppingGranularity, supportsInstructionBreakpoints, supportsExceptionFilterOptions, supportsSingleThreadExecutionRequests) -> {supportsBreakpointLocationsRequest; supportsClipboardContext; supportsSteppingGranularity; supportsInstructionBreakpoints; supportsExceptionFilterOptions; supportsSingleThreadExecutionRequests})
     (obj6
    (opt "supportsBreakpointLocationsRequest" bool)
    (opt "supportsClipboardContext" bool)
    (opt "supportsSteppingGranularity" bool)
    (opt "supportsInstructionBreakpoints" bool)
    (opt "supportsExceptionFilterOptions" bool)
    (opt "supportsSingleThreadExecutionRequests" bool))


    let make ?supportsBreakpointLocationsRequest ?supportsClipboardContext ?supportsSteppingGranularity ?supportsInstructionBreakpoints ?supportsExceptionFilterOptions ?supportsSingleThreadExecutionRequests () =
    {supportsBreakpointLocationsRequest; supportsClipboardContext; supportsSteppingGranularity; supportsInstructionBreakpoints; supportsExceptionFilterOptions; supportsSingleThreadExecutionRequests}

    end


    type t = (Capabilities_0.t * (Capabilities_10.t * (Capabilities_20.t * Capabilities_30.t)))

    let enc =
     let open Data_encoding in
     merge_objs Capabilities_0.enc @@ merge_objs Capabilities_10.enc @@ merge_objs Capabilities_20.enc Capabilities_30.enc

    let make ?supportsConfigurationDoneRequest ?supportsFunctionBreakpoints ?supportsConditionalBreakpoints ?supportsHitConditionalBreakpoints ?supportsEvaluateForHovers ?supportsStepBack ?supportsSetVariable ?supportsRestartFrame ?supportsGotoTargetsRequest ?supportsStepInTargetsRequest ?supportsCompletionsRequest ?completionTriggerCharacters ?supportsModulesRequest ?supportsRestartRequest ?supportsExceptionOptions ?supportsValueFormattingOptions ?supportsExceptionInfoRequest ?supportTerminateDebuggee ?supportSuspendDebuggee ?supportsDelayedStackTraceLoading ?supportsLoadedSourcesRequest ?supportsLogPoints ?supportsTerminateThreadsRequest ?supportsSetExpression ?supportsTerminateRequest ?supportsDataBreakpoints ?supportsReadMemoryRequest ?supportsWriteMemoryRequest ?supportsDisassembleRequest ?supportsCancelRequest ?supportsBreakpointLocationsRequest ?supportsClipboardContext ?supportsSteppingGranularity ?supportsInstructionBreakpoints ?supportsExceptionFilterOptions ?supportsSingleThreadExecutionRequests () =
    let t0 = Capabilities_0.make ?supportsConfigurationDoneRequest ?supportsFunctionBreakpoints ?supportsConditionalBreakpoints ?supportsHitConditionalBreakpoints ?supportsEvaluateForHovers ?supportsStepBack ?supportsSetVariable ?supportsRestartFrame ?supportsGotoTargetsRequest ?supportsStepInTargetsRequest () in

    let t1 = Capabilities_10.make ?supportsCompletionsRequest ?completionTriggerCharacters ?supportsModulesRequest ?supportsRestartRequest ?supportsExceptionOptions ?supportsValueFormattingOptions ?supportsExceptionInfoRequest ?supportTerminateDebuggee ?supportSuspendDebuggee ?supportsDelayedStackTraceLoading () in

    let t2 = Capabilities_20.make ?supportsLoadedSourcesRequest ?supportsLogPoints ?supportsTerminateThreadsRequest ?supportsSetExpression ?supportsTerminateRequest ?supportsDataBreakpoints ?supportsReadMemoryRequest ?supportsWriteMemoryRequest ?supportsDisassembleRequest ?supportsCancelRequest () in

    let t3 = Capabilities_30.make ?supportsBreakpointLocationsRequest ?supportsClipboardContext ?supportsSteppingGranularity ?supportsInstructionBreakpoints ?supportsExceptionFilterOptions ?supportsSingleThreadExecutionRequests () in

    (t0 , (t1 , (t2 , t3)))

    end


    type request =


    type response =


    type event = |}]


let%expect_test "Check anyOf example" =
  let schema_js = Ezjsonm.from_channel @@ open_in "data/anyof.json" in
  let dfs = Dfs.make ~schema_js in
  let actual = render dfs in
  Printf.printf "%s" actual;
  [%expect {|
    (* NOTE this file was autogenerated - do not modify by hand *)

    open Dap_t

    module Command = struct
    type t = Error

    let enc =
     let open Data_encoding in
     conv
     (function Error -> "error")
     (function "error" -> Error | _ -> failwith "Command")
     string

    end


    module Event = struct
    type t =

    let enc =
     let open Data_encoding in
     conv
     (function )
     (function _ -> failwith "Event")
     string

    end


    module SomeExample = struct
    type t = { moduleId: IntString.t option;
    adapterData: Data_encoding.json option; }

    let enc =
     let open Data_encoding in
     (* SomeExample.t *)
             conv
     (fun {moduleId; adapterData} -> (moduleId, adapterData))
     (fun (moduleId, adapterData) -> {moduleId; adapterData})
     (obj2
    (opt "moduleId" IntString.enc)
    (opt "adapterData" json))


    let make ?moduleId ?adapterData () =
    {moduleId; adapterData}

    end


    type request =


    type response =


    type event = |}]

let%expect_test "Check oneOf example" =
  let schema_js = Ezjsonm.from_channel @@ open_in "data/restartRequest.json" in
  let dfs = Dfs.make ~schema_js in
  let actual = render dfs in
  Printf.printf "%s" actual;
  [%expect {|
    (* NOTE this file was autogenerated - do not modify by hand *)

    open Dap_t

    module Command = struct
    type t = Restart | Error

    let enc =
     let open Data_encoding in
     conv
     (function Restart -> "restart" | Error -> "error")
     (function "restart" -> Restart | "error" -> Error | _ -> failwith "Command")
     string

    end


    module Event = struct
    type t =

    let enc =
     let open Data_encoding in
     conv
     (function )
     (function _ -> failwith "Event")
     string

    end


    module RestartRequestMessage = MakeRequest_optionalArgs (struct type t = Command.t let value=Command.Restart let enc = Command.enc end)

    type request =
    | RestartRequest of RestartArguments.t RestartRequestMessage.t

    type response =


    type event = |}]


let%expect_test "Check nullable example" =
  let schema_js = Ezjsonm.from_channel @@ open_in "data/nullableString.json" in
  let dfs = Dfs.make ~schema_js in
  let actual = render dfs in
  Printf.printf "%s" actual;
  [%expect {|
    (* NOTE this file was autogenerated - do not modify by hand *)

    open Dap_t

    module Command = struct
    type t = Error

    let enc =
     let open Data_encoding in
     conv
     (function Error -> "error")
     (function "error" -> Error | _ -> failwith "Command")
     string

    end


    module Event = struct
    type t =

    let enc =
     let open Data_encoding in
     conv
     (function )
     (function _ -> failwith "Event")
     string

    end


    module ExceptionDetails = struct
    type t = { message: string option;
    typeName: string option; }

    let enc =
     let open Data_encoding in
     (* ExceptionDetails.t *)
             conv
     (fun {message; typeName} -> (message, typeName))
     (fun (message, typeName) -> {message; typeName})
     (obj2
    (opt "message" string)
    (req "typeName" (option string)))


    let make ?message ~typeName () =
    {message; typeName}

    end


    type request =


    type response =


    type event = |}]


let%expect_test "Check valueFormat example" =
  let schema_js = Ezjsonm.from_channel @@ open_in "data/valueFormatExample.json" in
  let dfs = Dfs.make ~schema_js in
  let actual = render dfs in
  Printf.printf "%s" actual;
  [%expect {|
    (* NOTE this file was autogenerated - do not modify by hand *)

    open Dap_t

    module Command = struct
    type t = Error

    let enc =
     let open Data_encoding in
     conv
     (function Error -> "error")
     (function "error" -> Error | _ -> failwith "Command")
     string

    end


    module Event = struct
    type t =

    let enc =
     let open Data_encoding in
     conv
     (function )
     (function _ -> failwith "Event")
     string

    end


    module ValueFormat = struct
    type t = { hex: bool option; }

    let enc =
     let open Data_encoding in
     (* ValueFormat.t *)
             conv
     (fun {hex} -> hex)
     (fun hex -> {hex})
     (obj1
    (opt "hex" bool))


    let make ?hex () =
    {hex}

    end


    module StackFrameFormat = struct
    type t = { hex: bool option;
    parameters: bool option;
    parameterTypes: bool option;
    parameterNames: bool option;
    parameterValues: bool option;
    line: bool option;
    module_: bool option;
    includeAll: bool option; }

    let enc =
     let open Data_encoding in
     (* StackFrameFormat.t *)
             conv
     (fun {hex; parameters; parameterTypes; parameterNames; parameterValues; line; module_; includeAll} -> (hex, parameters, parameterTypes, parameterNames, parameterValues, line, module_, includeAll))
     (fun (hex, parameters, parameterTypes, parameterNames, parameterValues, line, module_, includeAll) -> {hex; parameters; parameterTypes; parameterNames; parameterValues; line; module_; includeAll})
     (obj8
    (opt "hex" bool)
    (opt "parameters" bool)
    (opt "parameterTypes" bool)
    (opt "parameterNames" bool)
    (opt "parameterValues" bool)
    (opt "line" bool)
    (opt "module" bool)
    (opt "includeAll" bool))


    let make ?hex ?parameters ?parameterTypes ?parameterNames ?parameterValues ?line ?module_ ?includeAll () =
    {hex; parameters; parameterTypes; parameterNames; parameterValues; line; module_; includeAll}

    end


    type request =


    type response =


    type event = |}]

let%expect_test "Check empty example" =
  let schema_js = Ezjsonm.from_channel @@ open_in "data/emptyObject.json" in
  let dfs = Dfs.make ~schema_js in
  let actual = render dfs in
  Printf.printf "%s" actual;
  [%expect {|
    (* NOTE this file was autogenerated - do not modify by hand *)

    open Dap_t

    module Command = struct
    type t = ConfigurationDone | Error

    let enc =
     let open Data_encoding in
     conv
     (function ConfigurationDone -> "configurationDone" | Error -> "error")
     (function "configurationDone" -> ConfigurationDone | "error" -> Error | _ -> failwith "Command")
     string

    end


    module Event = struct
    type t =

    let enc =
     let open Data_encoding in
     conv
     (function )
     (function _ -> failwith "Event")
     string

    end


    module ConfigurationDoneArguments = struct
    type t = unit

    let enc = Data_encoding.empty

    let make () = ()

    end


    module ConfigurationDoneRequestMessage = MakeRequest_optionalArgs (struct type t = Command.t let value=Command.ConfigurationDone let enc = Command.enc end)

    type request =
    | ConfigurationDoneRequest of ConfigurationDoneArguments.t ConfigurationDoneRequestMessage.t

    type response =


    type event = |}]


let%expect_test "Check LaunchResponse empty body  example" =
  let schema_js = Ezjsonm.from_channel @@ open_in "data/launchResponseEmptyExample.json" in
  let dfs = Dfs.make ~schema_js in
  let actual = render dfs in
  Printf.printf "%s" actual;
  [%expect {|
    (* NOTE this file was autogenerated - do not modify by hand *)

    open Dap_t

    module Command = struct
    type t = Error

    let enc =
     let open Data_encoding in
     conv
     (function Error -> "error")
     (function "error" -> Error | _ -> failwith "Command")
     string

    end


    module Event = struct
    type t =

    let enc =
     let open Data_encoding in
     conv
     (function )
     (function _ -> failwith "Event")
     string

    end


    module LaunchResponseMessage = MakeResponse_optionalBody (struct type t = Command.t let value=Command.Launch let enc = Command.enc end)

    type request =


    type response =
    | LaunchResponse of EmptyObject.t LaunchResponseMessage.t

    type event = |}]