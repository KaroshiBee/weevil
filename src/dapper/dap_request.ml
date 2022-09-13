open Dap_base

module Request = struct

  type t =
    | Cancel
    | Next
    | StackTrace
    | Scopes
    | Variables
    | Initialize
    | Attach
    | ConfigurationDone
    | Threads
    | Continue
    | Disconnect

  let enc_t =
    let open Data_encoding in
    conv
      (function | Cancel -> "cancel" | Next -> "next" | StackTrace -> "stackTrace" | Scopes -> "scopes" | Variables -> "variables" | Initialize -> "initialize" | Attach -> "attach" | ConfigurationDone -> "configurationDone" | Threads -> "threads" | Continue -> "continue" | Disconnect -> "disconnect")
      (function | "cancel" -> Cancel | "next" -> Next | "stackTrace" -> StackTrace | "scopes" -> Scopes | "variables" -> Variables | "initialize" -> Initialize | "attach" -> Attach | "configurationDone" -> ConfigurationDone | "threads" -> Threads | "continue" -> Continue | "disconnect" -> Disconnect | _ -> failwith "Unknown request")
      string

  type 'json cls_t = <
    ProtocolMessage.cls_t;
    command:t;
    arguments:'json
  >

  class ['json] cls
      (seq:int)
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
         (req "seq" int31)
         (req "type" ProtocolMessage.enc_t)
         (req "command" enc_t)
         (req "arguments" js)
      )

end

module Request_noargs = struct

  type t = Request.t

  type cls_t = <
    ProtocolMessage.cls_t;
    command:t;
  >

  class cls
      (seq:int)
      (command:t)
      = object
    inherit ProtocolMessage.cls seq Request

    method command = command

  end

  let enc =
    let open Data_encoding in
    conv
      (fun (r : < cls_t >) ->
         (r#seq, r#type_, r#command) )

      (fun (seq, _, command) ->
         new cls seq command)

      (obj3
         (req "seq" int31)
         (req "type" ProtocolMessage.enc_t)
         (req "command" Request.enc_t)
      )

end

module CancelArguments = struct

  type t = {
    requestId:int option;
    progressId:string option
  }

  let enc =
    let open Data_encoding in
    conv
      (fun {requestId; progressId} -> (requestId, progressId))
      (fun (requestId, progressId) -> {requestId; progressId})
      (obj2
         (opt "requestId" int31)
         (opt "progressId" string))

end


module CancelRequest = struct

  type args = CancelArguments.t

  type cls_t = args Request.cls_t

  class cls (seq:int) (arguments:args) = object
    inherit [args] Request.cls seq Cancel arguments
  end

  let enc = Request.enc CancelArguments.enc

end


module NextArguments = struct

  type t = {
    threadId: int;
    singleThread: bool option;
    granularity: SteppingGranularity.t option;
  }

  let enc =
    let open Data_encoding in
    conv
      (fun {threadId; singleThread; granularity} -> (threadId, singleThread, granularity))
      (fun (threadId, singleThread, granularity) -> {threadId; singleThread; granularity})
      (obj3
         (req "threadId" int31)
         (opt "singleThread" bool)
         (opt "granularity" SteppingGranularity.enc)
      )

end

module NextRequest = struct

  type args = NextArguments.t

  type cls_t = args Request.cls_t

  class cls (seq:int) (arguments:args) = object
    inherit [args] Request.cls seq Next arguments
  end

  let enc = Request.enc NextArguments.enc

end

(* class ['json] requester_cls (req:'json Request.cls_t) = object
 *   method req = req
 * end *)

module StackTraceArguments = struct

  type t = {
      threadId: int;
      startFrame: int option;
      levels: int option;
      (*  TODO format:  *)
  }

  let enc =
    let open Data_encoding in
    conv
      (fun {threadId; startFrame; levels} -> (threadId, startFrame, levels))
      (fun (threadId, startFrame, levels) -> {threadId; startFrame; levels})
      (obj3
         (req "threadId" int31)
         (opt "startFrame" int31)
         (opt "levels" int31)
      )

end

module StackTraceRequest = struct

  type args = StackTraceArguments.t

  type cls_t = args Request.cls_t

  class cls (seq:int) (arguments:args) = object
    inherit [args] Request.cls seq StackTrace arguments
  end

  let enc = Request.enc StackTraceArguments.enc

end


module ScopesArguments = struct
  type t = {
    frameId: int;
  }

  let enc =
    let open Data_encoding in
    conv
      (fun {frameId} -> frameId)
      (fun frameId -> {frameId})
      (obj1
         (req "frameId" int31)
      )

end

module ScopesRequest = struct

  type args = ScopesArguments.t

  type cls_t = args Request.cls_t

  class cls (seq:int) (arguments:args) = object
    inherit [args] Request.cls seq Scopes arguments
  end

  let enc = Request.enc ScopesArguments.enc

end


module VariablesArguments = struct
  type t = {
    variablesReference: int;
  }

  let enc =
    let open Data_encoding in
    conv
      (fun {variablesReference} -> variablesReference)
      (fun variablesReference -> {variablesReference})
      (obj1
         (req "variablesReference" int31)
      )

end

module VariablesRequest = struct

  type args = VariablesArguments.t

  type cls_t = args Request.cls_t

  class cls (seq:int) (arguments:args) = object
    inherit [args] Request.cls seq Variables arguments
  end

  let enc = Request.enc VariablesArguments.enc

end


module InitializeRequestArguments = struct
  type t0 = {
    clientID: string option;
    clientName: string option;
    adapterID: string;
    locale: string option;
    linesStartAt1: bool;
    columnsStartAt1: bool;
    pathFormat: string option; (* TODO *)
  }

  let enc_t0 =
    let open Data_encoding in
    conv
      ( fun {
          clientID;
          clientName;
          adapterID;
          locale;
          linesStartAt1;
          columnsStartAt1;
          pathFormat;
        } -> (
          clientID,
          clientName,
          adapterID,
          locale,
          linesStartAt1,
          columnsStartAt1,
          pathFormat
      ))
      ( fun (
          clientID,
          clientName,
          adapterID,
          locale,
          linesStartAt1,
          columnsStartAt1,
          pathFormat
      ) -> {
          clientID;
          clientName;
          adapterID;
          locale;
          linesStartAt1;
          columnsStartAt1;
          pathFormat;
        })
      (obj7
          (opt "clientID" string)
          (opt "clientName" string)
          (req "adapterID" string)
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
      ?clientID
      ?clientName
      ~adapterID
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
      clientID;
      clientName;
      adapterID;
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

  class cls (seq:int) (arguments:args) = object
    inherit [args] Request.cls seq Initialize arguments
  end

  let enc = Request.enc InitializeRequestArguments.enc

end


module AttachRequestArguments = struct

  type t = {
    type_: string option;
    request: Request.t option;
    mode: string option; (* TODO *)
    name: string option;
    host: string option;
    debugServer: int option;
  }

  let enc =
    let open Data_encoding in
    conv
      (fun {
         type_;
         request;
         mode;
         name;
         host;
         debugServer;
       } -> (
           type_,
           request,
           mode,
           name,
           host,
           debugServer
         ))
      (fun (
         type_,
         request,
         mode,
         name,
         host,
         debugServer
       ) ->  {
           type_;
           request;
           mode;
           name;
           host;
           debugServer;
         })
      (obj6
         (opt "type" string)
         (opt "request" Request.enc_t)
         (opt "mode" string)
         (opt "name" string)
         (opt "host" string)
         (opt "debugServer" int31)
         )

end


module AttachRequest = struct
  type args = AttachRequestArguments.t
  type cls_t = args Request.cls_t

  class cls (seq:int) (arguments:args) = object
    inherit [args] Request.cls seq Attach arguments
  end

  let enc = Request.enc AttachRequestArguments.enc
end


module ConfigurationDoneRequest = struct

  type cls_t = Request_noargs.cls_t

  class cls (seq:int) = object
    inherit Request_noargs.cls seq ConfigurationDone
  end

  let enc = Request_noargs.enc
end


module ThreadsRequest = struct

  type cls_t = Request_noargs.cls_t

  class cls (seq:int) = object
    inherit Request_noargs.cls seq Threads
  end

  let enc = Request_noargs.enc
end


module ContinueArguments = struct
  type t = {
    threadId: int;
    singleThread: bool option;
  }

  let enc =
    let open Data_encoding in
    conv
      (fun {threadId; singleThread} -> (threadId, singleThread))
      (fun (threadId, singleThread) -> {threadId; singleThread})
      (obj2
         (req "threadId" int31)
         (opt "singleThread" bool))
end


module ContinueRequest = struct
  type args = ContinueArguments.t
  type cls_t = args Request.cls_t

  class cls (seq:int) (arguments:args) = object
    inherit [args] Request.cls seq Continue arguments
  end

  let enc = Request.enc ContinueArguments.enc

end


module DisconnectArguments = struct
  type t = {
    restart: bool option;
    terminateDebugee: bool option;
    suspendDebuggee: bool option;
  }

  let enc =
    let open Data_encoding in
    conv
      (fun {
         restart;
         terminateDebugee;
         suspendDebuggee;
       } -> (
           restart,
           terminateDebugee,
           suspendDebuggee
         ))
      (fun (
         restart,
         terminateDebugee,
         suspendDebuggee
       ) -> {
           restart;
           terminateDebugee;
           suspendDebuggee;
         })
      (obj3
         (opt "restart" bool)
         (opt "terminateDebugee" bool)
         (opt "suspendDebuggee" bool)
      )

end


module DisconnectRequest = struct
  type args = DisconnectArguments.t
  type cls_t = args Request.cls_t

  class cls (seq:int) (arguments:args) = object
    inherit [args] Request.cls seq Disconnect arguments
  end

  let enc = Request.enc DisconnectArguments.enc

end
