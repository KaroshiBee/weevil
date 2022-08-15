open Dap_base


module Request = struct

  type t = Command_enum.t

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
         (req "type" ProtocolMessage.enc)
         (req "command" Command_enum.enc)
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
    granularity: SteppingGranularity_enum.t option;
  }

  let enc =
    let open Data_encoding in
    conv
      (fun {threadId; singleThread; granularity} -> (threadId, singleThread, granularity))
      (fun (threadId, singleThread, granularity) -> {threadId; singleThread; granularity})
      (obj3
         (req "threadId" int64)
         (opt "singleThread" bool)
         (opt "granularity" SteppingGranularity_enum.enc)
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
