open Dap_dfs

let%expect_test "Check ErrorResponse example" =
  let schema_js = Ezjsonm.from_channel @@ open_in "data/errorResponse.json" in
  let dfs = Dfs.make ~schema_js in
  let actual =
    Dfs.ordering dfs |> List.map (fun name ->
      let o = match (Hashtbl.find dfs.finished name) with | Sp.Object obj -> obj | _ -> assert false in
      try
        RenderResponse.(of_spec o |> render ~name)
      with _ ->
        RenderObject.(of_spec o |> render ~name)
      ) |> String.concat "\n\n"
  in
  Printf.printf "%s" actual;
  [%expect {|
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


    module ErrorResponse = MakeResponse (struct let command=Command.Error end) (ErrorResponse_body) |}]



let%expect_test "Check CancelRequest example" =
  let schema_js = Ezjsonm.from_channel @@ open_in "data/cancelRequest.json" in
  let dfs = Dfs.make ~schema_js in
  let actual =
    Dfs.ordering dfs |> List.map (fun name ->
      let o = match (Hashtbl.find dfs.finished name) with | Sp.Object obj -> obj | _ -> assert false in
      try
        RenderRequest.(of_spec o |> render ~name)
      with _ ->
        RenderObject.(of_spec o |> render ~name)
      ) |> String.concat "\n\n"
  in
  Printf.printf "%s" actual;
  [%expect {|
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


    module CancelRequest = MakeRequest_optionalArgs (struct let command=Command.Cancel end) (CancelArguments) |}]


let%expect_test "Check StoppedEvent example" =
  let schema_js = Ezjsonm.from_channel @@ open_in "data/stoppedEvent.json" in
  let dfs = Dfs.make ~schema_js in
  let actual =
    Dfs.ordering dfs |> List.map (fun name ->
        match (Hashtbl.find dfs.finished name) with
        | Sp.Object o -> (
          try
            RenderEvent.(of_spec o |> render ~name)
          with _ ->
            RenderObject.(of_spec o |> render ~name)
        )
        | Sp.Enum e -> (
            RenderEnum.(of_spec e |> render ~name)
        )
        | _ -> assert false
      ) |> String.concat "\n\n"
  in
  Printf.printf "%s" actual;
  [%expect {|
    module StoppedEvent_body_reason = struct
    type t = Step | Breakpoint | Exception | Pause | Entry | Goto | Function_breakpoint | Data_breakpoint | Instruction_breakpoint

    let enc =
     let open Data_encoding in
     conv
     (function Step -> "step" | Breakpoint -> "breakpoint" | Exception -> "exception" | Pause -> "pause" | Entry -> "entry" | Goto -> "goto" | Function_breakpoint -> "function breakpoint" | Data_breakpoint -> "data breakpoint" | Instruction_breakpoint -> "instruction breakpoint")
     (function "step" -> Step | "breakpoint" -> Breakpoint | "exception" -> Exception | "pause" -> Pause | "entry" -> Entry | "goto" -> Goto | "function breakpoint" -> Function_breakpoint | "data breakpoint" -> Data_breakpoint | "instruction breakpoint" -> Instruction_breakpoint | _ -> failwith "StoppedEvent_body_reason")
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


    module StoppedEvent = MakeEvent (struct let event=Event.Stopped end) (StoppedEvent_body) |}]
