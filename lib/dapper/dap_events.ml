(* NOTE autogenerated, do not manually edit *)
 type v = Memory | Invalidated | ProgressEnd | ProgressUpdate | ProgressStart | Capabilities | Process | LoadedSource | Module_ | Breakpoint | Output | Thread | Terminated | Exited | Continued | Stopped | Initialized [@@deriving eq]

 type 'a t = v

type memory
type invalidated
type progressEnd
type progressUpdate
type progressStart
type capabilities
type process
type loadedSource
type module_
type breakpoint
type output
type thread
type terminated
type exited
type continued
type stopped
type initialized

let equal = equal_v

let memory : memory t = Memory
let invalidated : invalidated t = Invalidated
let progressEnd : progressEnd t = ProgressEnd
let progressUpdate : progressUpdate t = ProgressUpdate
let progressStart : progressStart t = ProgressStart
let capabilities : capabilities t = Capabilities
let process : process t = Process
let loadedSource : loadedSource t = LoadedSource
let module_ : module_ t = Module_
let breakpoint : breakpoint t = Breakpoint
let output : output t = Output
let thread : thread t = Thread
let terminated : terminated t = Terminated
let exited : exited t = Exited
let continued : continued t = Continued
let stopped : stopped t = Stopped
let initialized : initialized t = Initialized

let to_string (type a) : a t -> string = function | (Memory : memory t) -> "memory"
| (Invalidated : invalidated t) -> "invalidated"
| (ProgressEnd : progressEnd t) -> "progressEnd"
| (ProgressUpdate : progressUpdate t) -> "progressUpdate"
| (ProgressStart : progressStart t) -> "progressStart"
| (Capabilities : capabilities t) -> "capabilities"
| (Process : process t) -> "process"
| (LoadedSource : loadedSource t) -> "loadedSource"
| (Module_ : module_ t) -> "module"
| (Breakpoint : breakpoint t) -> "breakpoint"
| (Output : output t) -> "output"
| (Thread : thread t) -> "thread"
| (Terminated : terminated t) -> "terminated"
| (Exited : exited t) -> "exited"
| (Continued : continued t) -> "continued"
| (Stopped : stopped t) -> "stopped"
| (Initialized : initialized t) -> "initialized"

let from_string (type a) : string -> a t = function | "memory" -> (Memory : memory t)
| "invalidated" -> (Invalidated : invalidated t)
| "progressEnd" -> (ProgressEnd : progressEnd t)
| "progressUpdate" -> (ProgressUpdate : progressUpdate t)
| "progressStart" -> (ProgressStart : progressStart t)
| "capabilities" -> (Capabilities : capabilities t)
| "process" -> (Process : process t)
| "loadedSource" -> (LoadedSource : loadedSource t)
| "module" -> (Module_ : module_ t)
| "breakpoint" -> (Breakpoint : breakpoint t)
| "output" -> (Output : output t)
| "thread" -> (Thread : thread t)
| "terminated" -> (Terminated : terminated t)
| "exited" -> (Exited : exited t)
| "continued" -> (Continued : continued t)
| "stopped" -> (Stopped : stopped t)
| "initialized" -> (Initialized : initialized t)| _ -> failwith "Dap_events"



let enc ~value =
  let open Data_encoding in
  let to_str = to_string in
  let from_str =
    let sentinal = to_string value in
    function
    | s when s = sentinal ->
      Ok (from_string s)
    | _  as s ->
      let err = Printf.sprintf "expected '%s', got '%s'" sentinal s in
      Error err
  in
  conv_with_guard
    to_str from_str string
      