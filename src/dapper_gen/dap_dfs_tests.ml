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
    type t = { lines: int list option;
    urlLabel: string option;
    url: string option;
    showUser: bool option;
    sendTelemetry: bool option;
    variables: Data_encoding.json option;
    format: string;
    id: int; }

    let enc =
     let open Data_encoding in
     conv
     (fun {lines; urlLabel; url; showUser; sendTelemetry; variables; format; id} -> (lines, urlLabel, url, showUser, sendTelemetry, variables, format, id))
     (fun (lines, urlLabel, url, showUser, sendTelemetry, variables, format, id) -> {lines; urlLabel; url; showUser; sendTelemetry; variables; format; id})
     (obj8
    (opt "lines" (list int31))
    (opt "urlLabel" string)
    (opt "url" string)
    (opt "showUser" bool)
    (opt "sendTelemetry" bool)
    (opt "variables" json)
    (req "format" string)
    (req "id" int31))


    let make ?lines ?urlLabel ?url ?showUser ?sendTelemetry ?variables ~format ~id () =
    {lines; urlLabel; url; showUser; sendTelemetry; variables; format; id}

    end


    module ErrorResponse_body = struct
    type t = { error: Message.t option; }

    let enc =
     let open Data_encoding in
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
    type t = { progressId: string option;
    requestId: int option; }

    let enc =
     let open Data_encoding in
     conv
     (fun {progressId; requestId} -> (progressId, requestId))
     (fun (progressId, requestId) -> {progressId; requestId})
     (obj2
    (opt "progressId" string)
    (opt "requestId" int31))


    let make ?progressId ?requestId () =
    {progressId; requestId}

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
    type t = { hitBreakpointIds: int list option;
    allThreadsStopped: bool option;
    text: string option;
    preserveFocusHint: bool option;
    threadId: int option;
    description: string option;
    reason: StoppedEvent_body_reason.t; }

    let enc =
     let open Data_encoding in
     conv
     (fun {hitBreakpointIds; allThreadsStopped; text; preserveFocusHint; threadId; description; reason} -> (hitBreakpointIds, allThreadsStopped, text, preserveFocusHint, threadId, description, reason))
     (fun (hitBreakpointIds, allThreadsStopped, text, preserveFocusHint, threadId, description, reason) -> {hitBreakpointIds; allThreadsStopped; text; preserveFocusHint; threadId; description; reason})
     (obj7
    (opt "hitBreakpointIds" (list int31))
    (opt "allThreadsStopped" bool)
    (opt "text" string)
    (opt "preserveFocusHint" bool)
    (opt "threadId" int31)
    (opt "description" string)
    (req "reason" StoppedEvent_body_reason.enc))


    let make ?hitBreakpointIds ?allThreadsStopped ?text ?preserveFocusHint ?threadId ?description ~reason () =
    {hitBreakpointIds; allThreadsStopped; text; preserveFocusHint; threadId; description; reason}

    end


    module StoppedEventMessage = MakeEvent (struct type t = Event.t let value=Event.Stopped let enc = Event.enc end)

    type request =


    type response =


    type event =
    | StoppedEvent of StoppedEvent_body.t StoppedEventMessage.t |}]
