open Dap_dfs

let%expect_test "Check ErrorResponse example" =
  let schema_js = Ezjsonm.from_channel @@ open_in "data/errorResponse.json" in
  let dfs = Dfs.make ~schema_js in
  let actual = render dfs in
  Printf.printf "%s" actual;
  [%expect {|
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


    module CancelRequestMessage = MakeRequest (struct type t = Command.t let value=Command.Cancel let enc = Command.enc end)

    type request =
    | CancelRequest of CancelRequest_command.t CancelRequestMessage.t

    type response =


    type event = |}]


let%expect_test "Check StoppedEvent example" =
  let schema_js = Ezjsonm.from_channel @@ open_in "data/stoppedEvent.json" in
  let dfs = Dfs.make ~schema_js in
  let actual = render dfs in
  Printf.printf "%s" actual;
  [%expect {|
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
    | StoppedEvent of StoppedEvent_event.t StoppedEventMessage.t |}]

let%expect_test "Check cyclic example" =
  let schema_js = Ezjsonm.from_channel @@ open_in "data/cyclic.json" in
  let dfs = Dfs.make ~schema_js in
  let actual = render dfs in
  Printf.printf "%s" actual;
  [%expect {|
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
