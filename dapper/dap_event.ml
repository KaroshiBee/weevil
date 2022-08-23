open Dap_base


module Event = struct

  type t = Dap_base.Event.t

  type 'json cls_t = <
    ProtocolMessage.cls_t;
    event:t;
    body:'json
  >

  class ['json] cls
      (seq:int64)
      (event:t)
      (body:'json)
      = object
    inherit ProtocolMessage.cls seq Event

    method event = event
    method body = body

  end

  let enc js =
    let open Data_encoding in
    conv
      (fun (r : < 'json cls_t >) ->
         (r#seq, r#type_, r#event, r#body) )

      (fun (seq, _, event, body) ->
         new cls seq event body)

      (obj4
         (req "seq" int64)
         (req "type" ProtocolMessage.enc)
         (req "event" Dap_base.Event.enc)
         (req "body" js)
      )

end


module InitializedEvent = struct

  type cls_t = unit Event.cls_t

  class cls (seq:int64) = object
    inherit [unit] Event.cls seq Initialized ()
  end

  let enc = Event.enc (Data_encoding.unit)

end


module StoppedEvent = struct

  type body = {
    reason: StoppedEvent_1_body_reason.t;
    description: string option;
    threadId: int64 option;
    preserveFocusHint: bool option;
    text: string option;
    allThreadsStopped: bool option;
    hitBreakpointIds: int64 list option;
  }

  let body ~reason ?description ?threadId ?preserveFocusHint ?text ?allThreadsStopped ?hitBreakpointIds () = {
    reason;
    description;
    threadId;
    preserveFocusHint;
    text;
    allThreadsStopped;
    hitBreakpointIds;
  }

  type cls_t = body Event.cls_t

  class cls (seq:int64) (body:body) = object
    inherit [body] Event.cls seq Stopped body
  end

  let enc =
    let open Data_encoding in
    Event.enc @@
    conv
      (fun {
         reason;
         description;
         threadId;
         preserveFocusHint;
         text;
         allThreadsStopped;
         hitBreakpointIds
       } -> (
           reason,
           description,
           threadId,
           preserveFocusHint,
           text,
           allThreadsStopped,
           hitBreakpointIds
         ))
      (fun (
           reason,
           description,
           threadId,
           preserveFocusHint,
           text,
           allThreadsStopped,
           hitBreakpointIds
         ) -> {
         reason;
         description;
         threadId;
         preserveFocusHint;
         text;
         allThreadsStopped;
         hitBreakpointIds
       })
      (obj7
         (req "reason" StoppedEvent_1_body_reason.enc)
         (opt "description" string)
         (opt "threadId" int64)
         (opt "preserveFocusHint" bool)
         (opt "text" string)
         (opt "allThreadsStopped" bool)
         (opt "hitBreakpointIds" (list int64))
         )

end


module ContinuedEvent = struct

  type body = {
    threadId: int64;
    allThreadsContinued: bool option;
  }

  type cls_t = body Event.cls_t

  class cls (seq:int64) (body:body) = object
    inherit [body] Event.cls seq Continued body
  end

  let enc =
    let open Data_encoding in
    Event.enc @@
    conv
      (fun {threadId; allThreadsContinued} -> (threadId, allThreadsContinued))
      (fun (threadId, allThreadsContinued) -> {threadId; allThreadsContinued})
      (obj2
         (req "threadId" int64)
         (opt "allThreadsContinued" bool))

end


module ExitedEvent = struct

  type body = {
    exitCode: int64;
  }

  type cls_t = body Event.cls_t

  class cls (seq:int64) (body:body) = object
    inherit [body] Event.cls seq Exited body
  end

  let enc =
    let open Data_encoding in
    Event.enc @@
    conv
      (fun {exitCode} -> exitCode)
      (fun exitCode -> {exitCode})
      (obj1
         (req "exitCode" int64))

end


module TerminatedEvent = struct

  type 'json body = {
    restart: 'json
  }

  type 'json cls_t = 'json body option Event.cls_t

  class ['json] cls (seq:int64) (body:'json body option) = object
    inherit ['json body option] Event.cls seq Terminated body
  end

  let enc =
    let open Data_encoding in
    Event.enc @@
    conv
      (fun {restart} -> restart)
      (fun restart -> {restart})
      (obj1
         (req "restart" json))

end


module ThreadEvent = struct

  type body = {
    reason: ThreadEvent_1_body_reason.t;
    threadId: int64;
  }

  type cls_t = body Event.cls_t

  class cls (seq:int64) (body:body) = object
    inherit [body] Event.cls seq Thread body
  end

  let enc =
    let open Data_encoding in
    Event.enc @@
    conv
      (fun {reason; threadId} -> (reason, threadId))
      (fun (reason, threadId) -> {reason; threadId})
      (obj2
         (req "reason" ThreadEvent_1_body_reason.enc)
         (req "threadId" int64))

end


module OutputEvent = struct

  type 'json body = {
    output: string;
    category: OutputEvent_1_body_category.t option;
    group: OutputEvent_1_body_group.t option;
    variablesReference: int64 option;
    source: 'json Source.t option;
    line: int64 option;
    column: int64 option;
    data: 'json option;
  }

  type 'json cls_t = 'json body Event.cls_t

  class ['json] cls
      (seq:int64)
      (body:'json body) = object
    inherit ['json body] Event.cls seq Output body
  end

  let enc =
    let open Data_encoding in
    Event.enc @@
    conv
      (fun {
         output;
         category;
         group;
         variablesReference;
         source;
         line;
         column;
         data
       } -> (
           output,
           category,
           group,
           variablesReference,
           source,
           line,
           column,
           data
         )
      )
      (fun (
         output,
         category,
         group,
         variablesReference,
         source,
         line,
         column,
         data
       )
         -> {
             output;
             category;
             group;
             variablesReference;
             source;
             line;
             column;
             data
           }
      )
      (obj8
         (req "output" string)
         (opt "category" OutputEvent_1_body_category.enc)
         (opt "group" OutputEvent_1_body_group.enc)
         (opt "variablesReference" int64)
         (opt "source" (Source.enc json))
         (opt "line" int64)
         (opt "column" int64)
         (opt "data" json)
      )

end


module BreakpointEvent = struct

  type 'json body = {
    reason: BreakpointEvent_1_body_reason.t;
    breakpoint: 'json Breakpoint.t;
  }

  type 'json cls_t = 'json body Event.cls_t

  class ['json] cls (seq:int64) (body:'json body) = object
    inherit ['json body] Event.cls seq Breakpoint body
  end

  let enc =
    let open Data_encoding in
    Event.enc @@
    conv
      (fun {
         reason;
         breakpoint;
       } -> (
           reason,
           breakpoint
         )
      )
      (fun (
         reason,
         breakpoint
       ) -> {
           reason;
           breakpoint;
         }
      )
      (obj2
         (req "reason" BreakpointEvent_1_body_reason.enc)
         (req "breakpoint" Breakpoint.enc)
      )

end


module ModuleEvent = struct

  type body = {
    reason: ModuleEvent_1_body_reason.t;
    module_: Module_.t
  }

  type cls_t = body Event.cls_t

  class cls (seq:int64) (body:body) = object
    inherit [body] Event.cls seq Module_ body
  end

  let enc =
    let open Data_encoding in
    Event.enc @@
    conv
      (fun {
         reason;
         module_;
       } -> (
           reason,
           module_
         )
      )
      (fun (
         reason,
         module_
       ) -> {
           reason;
           module_;
         }
      )
      (obj2
         (req "reason" ModuleEvent_1_body_reason.enc)
         (req "module" Module_.enc)
      )

end


module LoadedSourceEvent = struct

  type 'json body = {
    reason: LoadedSourceEvent_1_body_reason.t;
    source: 'json Source.t;
  }

  type 'json cls_t = 'json body Event.cls_t

  class ['json] cls (seq:int64) (body:'json body) = object
    inherit ['json body] Event.cls seq LoadedSource body
  end

  let enc =
    let open Data_encoding in
    Event.enc @@
    conv
      (fun {
         reason;
         source;
       } -> (
           reason,
           source
         )
      )
      (fun (
         reason,
         source
       ) -> {
           reason;
           source;
         }
      )
      (obj2
         (req "reason" LoadedSourceEvent_1_body_reason.enc)
         (req "source" @@ Source.enc json)
      )

end


module ProcessEvent = struct

  type body = {
    name: string;
    systemProcessId: int64 option;
    isLocalProcess: bool option;
    startMethod: ProcessEvent_1_body_startMethod.t option;
    pointerSize: int64 option;
  }

  type cls_t = body Event.cls_t

  class cls (seq:int64) (body:body) = object
    inherit [body] Event.cls seq Process body
  end

  let enc =
    let open Data_encoding in
    Event.enc @@
    conv
      (fun {
         name;
         systemProcessId;
         isLocalProcess;
         startMethod;
         pointerSize;
       } -> (
           name,
           systemProcessId,
           isLocalProcess,
           startMethod,
           pointerSize
         )
      )
      (fun (
         name,
         systemProcessId,
         isLocalProcess,
         startMethod,
         pointerSize
       ) -> {
           name;
           systemProcessId;
           isLocalProcess;
           startMethod;
           pointerSize;
         }
      )
      (obj5
         (req "name" string)
         (opt "systemProcessId" int64)
         (opt "isLocalProcess" bool)
         (opt "startMethod" ProcessEvent_1_body_startMethod.enc)
         (opt "pointerSize" int64)
      )

end


module CapabilitiesEvent = struct

  type body = {
    capabilities: Capabilities.t
  }

  type cls_t = body Event.cls_t

  class cls (seq:int64) (body:body) = object
    inherit [body] Event.cls seq Capabilities body
  end

  let enc =
    let open Data_encoding in
    Event.enc @@
    conv
      (fun {capabilities} -> capabilities)
      (fun capabilities -> {capabilities})
      (obj1
         (req "capabilities" Capabilities.enc)
      )

end


module ProgressStartEvent = struct

  type body = {
    progressId: string;
    title: string;
    requestId: int64 option;
    cancellable: bool option;
    message: string option;
    percentage: int option;
  }

  type cls_t = body Event.cls_t

  class cls (seq:int64) (body:body) = object
    inherit [body] Event.cls seq ProgressStart body
  end

  let enc =
    let open Data_encoding in
    Event.enc @@
    conv
      (fun {
         progressId;
         title;
         requestId;
         cancellable;
         message;
         percentage;

       } -> (
           progressId,
           title,
           requestId,
           cancellable,
           message,
           percentage
         ))
      (fun  (
         progressId,
         title,
         requestId,
         cancellable,
         message,
         percentage
       ) -> {
           progressId;
           title;
           requestId;
           cancellable;
           message;
           percentage;

         })
      (obj6
         (req "progressId" string)
         (req "title" string)
         (opt "requestId" int64)
         (opt "cancellable" bool)
         (opt "message" string)
         (opt "percentage" @@ ranged_int 0 100)
      )


end


module ProgressUpdateEvent = struct

  type body = {
    progressId: string;
    message: string option;
    percentage: int option;
  }

  type cls_t = body Event.cls_t

  class cls (seq:int64) (body:body) = object
    inherit [body] Event.cls seq ProgressUpdate body
  end

  let enc =
    let open Data_encoding in
    Event.enc @@
    conv
      (fun {progressId; message; percentage} -> (progressId, message, percentage))
      (fun (progressId, message, percentage) -> {progressId; message; percentage})
      (obj3
         (req "progressId" string)
         (opt "message" string)
         (opt "percentage" (ranged_int 0 100))
      )

end


module ProgressEndEvent = struct

  type body = {
    progressId: string;
    message: string option;
  }

  type cls_t = body Event.cls_t

  class cls (seq:int64) (body:body) = object
    inherit [body] Event.cls seq ProgressEnd body
  end

  let enc =
    let open Data_encoding in
    Event.enc @@
    conv
      (fun {progressId; message} -> (progressId, message))
      (fun (progressId, message) -> {progressId; message})
      (obj2
         (req "progressId" string)
         (opt "message" string)
      )

end


module InvalidatedEvent = struct

  type body = {
    areas: InvalidatedAreas.t list option;
    threadId: int64 option;
    stackFrameId: int64 option;

  }

  type cls_t = body Event.cls_t

  class cls (seq:int64) (body:body) = object
    inherit [body] Event.cls seq Invalidated body
  end

  let enc =
    let open Data_encoding in
    Event.enc @@
    conv
      (fun {areas; threadId; stackFrameId} -> (areas, threadId, stackFrameId))
      (fun (areas, threadId, stackFrameId) -> {areas; threadId; stackFrameId})
      (obj3
         (opt "areas" (list InvalidatedAreas.enc))
         (opt "threadId" int64)
         (opt "stackFrameId" int64)
      )

end


module MemoryEvent = struct

  type body = {
    memoryReference: string;
    offset: int64;
    count: int64;
  }

  type cls_t = body Event.cls_t

  class cls (seq:int64) (body:body) = object
    inherit [body] Event.cls seq Memory body
  end

  let enc =
    let open Data_encoding in
    Event.enc @@
    conv
      (fun {memoryReference; offset; count} -> (memoryReference, offset, count))
      (fun (memoryReference, offset, count) -> {memoryReference; offset; count})
      (obj3
         (req "memoryReference" string)
         (req "offset" int64)
         (req "count" int64)
      )

end
