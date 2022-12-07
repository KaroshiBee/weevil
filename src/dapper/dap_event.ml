include Dap_message.Event
module Message = Dap_message.EventMessage

let default_event_req event body =
  Message.make ~seq:Dap_base.Seqr.not_set ~event ~body ()

let default_event_opt event body =
  Message.make_opt ~seq:Dap_base.Seqr.not_set ~event ~body ()

type _ expr =
  | Val : 'msg t -> 'msg expr
  | Map : ('msg -> 'b) expr * 'msg expr -> 'b expr
  | Equal : ('msg1 -> 'msg2 -> bool) expr * 'msg1 expr * 'msg2 expr -> bool expr

(* stuff that is used in handlers *)
let fmap_ f = Fmap f
let val_ x = Val x
let map_ (f, x) = Map (f, x)

(* currently only used for tests *)
let eq_ f = Eq f
let equal_ (f, x, y) = Equal (f, x, y)

let rec eval : type msg. msg expr -> msg = function
  | Val (Eq f) -> f
  | Val (Fmap f) -> f
  | Val (InitializedEvent msg) -> msg
  | Val (StoppedEvent msg) -> msg
  | Val (ContinuedEvent msg) -> msg
  | Val (ExitedEvent msg) -> msg
  | Val (TerminatedEvent msg) -> msg
  | Val (ThreadEvent msg) -> msg
  | Val (OutputEvent msg) -> msg
  | Val (BreakpointEvent msg) -> msg
  | Val (ModuleEvent msg) -> msg
  | Val (LoadedSourceEvent msg) -> msg
  | Val (ProcessEvent msg) -> msg
  | Val (CapabilitiesEvent msg) -> msg
  | Val (ProgressStartEvent msg) -> msg
  | Val (ProgressUpdateEvent msg) -> msg
  | Val (ProgressEndEvent msg) -> msg
  | Val (InvalidatedEvent msg) -> msg
  | Val (MemoryEvent msg) -> msg
  | Map (f, v) -> let f' = (eval f) and v' = eval v in (f' v')
  | Equal (eq, v1, v2) -> let eq' = (eval eq) and v1' = eval v1 and v2' = eval v2 in (eq' v1' v2')
