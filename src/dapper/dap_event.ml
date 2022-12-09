include Dap_messages.Event
module Message = Dap_messages.EventMessage

let default_event_req event body =
  Message.make ~seq:Dap_base.Seqr.not_set ~event ~body ()

let default_event_opt event body =
  Message.make_opt ~seq:Dap_base.Seqr.not_set ~event ~body ()

type _ expr =
  | Val : 'msg t -> 'msg expr
  | Map : ('msg -> 'a) expr * 'msg expr -> 'a expr

let rec eval : type msg. msg expr -> msg = function
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

let map_f ~f x = Map (Val (Fmap f), Val x)
let map2_f ~f x y =
  let f' x' = fun y' -> f x' y' in
  let f' = eval @@ map_f ~f:f' x in
  map_f ~f:f' y
let equal ~equal_f = map2_f ~f:equal_f
