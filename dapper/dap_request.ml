open Dap_base

module Request = struct

  type t =
    | Cancel
    | Next
    | StackTrace
    | Scopes
    | Variables
    | Initialize

  let enc_t =
    let open Data_encoding in
    conv
      (function | Cancel -> "cancel" | Next -> "next" | StackTrace -> "stackTrace" | Scopes -> "scopes" | Variables -> "variables" | Initialize -> "initialize")
      (function | "cancel" -> Cancel | "next" -> Next | "stackTrace" -> StackTrace | "scopes" -> Scopes | "variables" -> Variables | "initialize" -> Initialize | _ -> failwith "Unknown request")
      string

  type 'json cls_t = <
    ProtocolMessage.cls_t;
    command:t;
    arguments:'json
  >

  class ['json] cls
      (seq:int64)
      (command:t)
      (arguments:'json)
      = object
    inherit ProtocolMessage.cls seq Request

    method command = command
    method arguments = arguments

  end

  let enc js =
    let open Data_encoding in
    conv
      (fun (r : < 'json cls_t >) ->
         (r#seq, r#type_, r#command, r#arguments) )

      (fun (seq, _, command, arguments) ->
         new cls seq command arguments)

      (obj4
         (req "seq" int64)
         (req "type" ProtocolMessage.enc_t)
         (req "command" enc_t)
         (req "arguments" js)
      )

end

module CancelArguments = struct

  type t = {
    requestId:int64 option;
    progressId:string option
  }

  let enc =
    let open Data_encoding in
    conv
      (fun {requestId; progressId} -> (requestId, progressId))
      (fun (requestId, progressId) -> {requestId; progressId})
      (obj2
         (opt "requestId" int64)
         (opt "progressId" string))

end


module CancelRequest = struct

  type args = CancelArguments.t

  type cls_t = args Request.cls_t

  class cls (seq:int64) (arguments:args) = object
    inherit [args] Request.cls seq Cancel arguments
  end

  let enc = Request.enc CancelArguments.enc

end


module NextArguments = struct

  type t = {
    threadId: int64;
    singleThread: bool option;
    granularity: SteppingGranularity.t option;
  }

  let enc =
    let open Data_encoding in
    conv
      (fun {threadId; singleThread; granularity} -> (threadId, singleThread, granularity))
      (fun (threadId, singleThread, granularity) -> {threadId; singleThread; granularity})
      (obj3
         (req "threadId" int64)
         (opt "singleThread" bool)
         (opt "granularity" SteppingGranularity.enc)
      )

end

module NextRequest = struct

  type args = NextArguments.t

  type cls_t = args Request.cls_t

  class cls (seq:int64) (arguments:args) = object
    inherit [args] Request.cls seq Next arguments
  end

  let enc = Request.enc NextArguments.enc

end

(* class ['json] requester_cls (req:'json Request.cls_t) = object
 *   method req = req
 * end *)

module StackTraceArguments = struct

  type t = {
      threadId: int64;
      startFrame: int64 option;
      levels: int64 option;
      (*  TODO format:  *)
  }

  let enc =
    let open Data_encoding in
    conv
      (fun {threadId; startFrame; levels} -> (threadId, startFrame, levels))
      (fun (threadId, startFrame, levels) -> {threadId; startFrame; levels})
      (obj3
         (req "threadId" int64)
         (opt "startFrame" int64)
         (opt "levels" int64)
      )

end

module StackTraceRequest = struct

  type args = StackTraceArguments.t

  type cls_t = args Request.cls_t

  class cls (seq:int64) (arguments:args) = object
    inherit [args] Request.cls seq StackTrace arguments
  end

  let enc = Request.enc StackTraceArguments.enc

end


module ScopesArguments = struct
  type t = {
    frameId: int64;
  }

  let enc =
    let open Data_encoding in
    conv
      (fun {frameId} -> frameId)
      (fun frameId -> {frameId})
      (obj1
         (req "frameId" int64)
      )

end

module ScopesRequest = struct

  type args = ScopesArguments.t

  type cls_t = args Request.cls_t

  class cls (seq:int64) (arguments:args) = object
    inherit [args] Request.cls seq Scopes arguments
  end

  let enc = Request.enc ScopesArguments.enc

end


module VariablesArguments = struct
  type t = {
    variablesReference: int64;
  }

  let enc =
    let open Data_encoding in
    conv
      (fun {variablesReference} -> variablesReference)
      (fun variablesReference -> {variablesReference})
      (obj1
         (req "variablesReference" int64)
      )

end

module VariablesRequest = struct

  type args = VariablesArguments.t

  type cls_t = args Request.cls_t

  class cls (seq:int64) (arguments:args) = object
    inherit [args] Request.cls seq Variables arguments
  end

  let enc = Request.enc VariablesArguments.enc

end


module InitializeRequestArguments = struct
  type t0 = {
    clientId: string option;
    clientName: string option;
    adapterId: string;
    locale: string option;
    linesStartAt1: bool;
    columnsStartAt1: bool;
    pathFormat: string option; (* TODO *)
  }

  let enc_t0 =
    let open Data_encoding in
    conv
      ( fun {
          clientId;
          clientName;
          adapterId;
          locale;
          linesStartAt1;
          columnsStartAt1;
          pathFormat;
        } -> (
          clientId,
          clientName,
          adapterId,
          locale,
          linesStartAt1,
          columnsStartAt1,
          pathFormat
      ))
      ( fun (
          clientId,
          clientName,
          adapterId,
          locale,
          linesStartAt1,
          columnsStartAt1,
          pathFormat
      ) -> {
          clientId;
          clientName;
          adapterId;
          locale;
          linesStartAt1;
          columnsStartAt1;
          pathFormat;
        })
      (obj7
          (opt "clientId" string)
          (opt "clientName" string)
          (req "adapterId" string)
          (opt "locale" string)
          (req "linesStartAt1" bool)
          (req "columnsStartAt1" bool)
          (opt "pathFormat" string)
      )

  type t1 = {
    supportsVariableType: bool option;
    supportsVariablePaging: bool option;
    supportsRunInTerminalRequest: bool option;
    supportsMemoryReferences: bool option;
    supportsProgressreporting: bool option;
    supportsInvalidatedEvent: bool option;
    supportsMemoryEvent: bool option;
    supportsArgsCanBeInterpretedByShell: bool option;
  }

  let enc_t1 =
    let open Data_encoding in
    conv
      ( fun {
          supportsVariableType;
          supportsVariablePaging;
          supportsRunInTerminalRequest;
          supportsMemoryReferences;
          supportsProgressreporting;
          supportsInvalidatedEvent;
          supportsMemoryEvent;
          supportsArgsCanBeInterpretedByShell;
        } -> (
            supportsVariableType,
            supportsVariablePaging,
            supportsRunInTerminalRequest,
            supportsMemoryReferences,
            supportsProgressreporting,
            supportsInvalidatedEvent,
            supportsMemoryEvent,
            supportsArgsCanBeInterpretedByShell

          )

      )
      ( fun (
          supportsVariableType,
          supportsVariablePaging,
          supportsRunInTerminalRequest,
          supportsMemoryReferences,
          supportsProgressreporting,
          supportsInvalidatedEvent,
          supportsMemoryEvent,
          supportsArgsCanBeInterpretedByShell
        ) -> {
            supportsVariableType;
            supportsVariablePaging;
            supportsRunInTerminalRequest;
            supportsMemoryReferences;
            supportsProgressreporting;
            supportsInvalidatedEvent;
            supportsMemoryEvent;
            supportsArgsCanBeInterpretedByShell;
          }
      )
      (obj8
         (opt "supportsVariableType" bool)
         (opt "supportsVariablePaging" bool)
         (opt "supportsRunInTerminalRequest" bool)
         (opt "supportsMemoryReferences" bool)
         (opt "supportsProgressreporting" bool)
         (opt "supportsInvalidatedEvent" bool)
         (opt "supportsMemoryEvent" bool)
         (opt "supportsArgsCanBeInterpretedByShell" bool)
      )

  type t = t0 * t1

  let enc =
    let open Data_encoding in
    merge_objs enc_t0 enc_t1

  let make
      ?clientId
      ?clientName
      ~adapterId
      ?locale
      ~linesStartAt1
      ~columnsStartAt1
      ?pathFormat
      ?supportsVariableType
      ?supportsVariablePaging
      ?supportsRunInTerminalRequest
      ?supportsMemoryReferences
      ?supportsProgressreporting
      ?supportsInvalidatedEvent
      ?supportsMemoryEvent
      ?supportsArgsCanBeInterpretedByShell
      () =
    {
      clientId;
      clientName;
      adapterId;
      locale;
      linesStartAt1;
      columnsStartAt1;
      pathFormat;
    }, {
      supportsVariableType;
      supportsVariablePaging;
      supportsRunInTerminalRequest;
      supportsMemoryReferences;
      supportsProgressreporting;
      supportsInvalidatedEvent;
      supportsMemoryEvent;
      supportsArgsCanBeInterpretedByShell;
    }

end


module InitializeRequest = struct
  type args = InitializeRequestArguments.t
  type cls_t = args Request.cls_t

  class cls (seq:int64) (arguments:args) = object
    inherit [args] Request.cls seq Initialize arguments
  end

      let enc = Request.enc InitializeRequestArguments.enc

end
