(* NOTE autogenerating Event, do not manually edit *)
type v = Memory | Invalidated | ProgressEnd | ProgressUpdate | ProgressStart | Capabilities | Process | LoadedSource | Module_ | Breakpoint | Output | Thread | Terminated | Exited | Continued | Stopped | Initialized

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

let f : type a. a t -> string = function | (Memory : _ t) -> "memory"
| (Invalidated : _ t) -> "invalidated"
| (ProgressEnd : _ t) -> "progressEnd"
| (ProgressUpdate : _ t) -> "progressUpdate"
| (ProgressStart : _ t) -> "progressStart"
| (Capabilities : _ t) -> "capabilities"
| (Process : _ t) -> "process"
| (LoadedSource : _ t) -> "loadedSource"
| (Module_ : _ t) -> "module"
| (Breakpoint : _ t) -> "breakpoint"
| (Output : _ t) -> "output"
| (Thread : _ t) -> "thread"
| (Terminated : _ t) -> "terminated"
| (Exited : _ t) -> "exited"
| (Continued : _ t) -> "continued"
| (Stopped : _ t) -> "stopped"
| (Initialized : _ t) -> "initialized"

let g : type a. string -> a t = function | "memory" -> (Memory : _ t)
| "invalidated" -> (Invalidated : _ t)
| "progressEnd" -> (ProgressEnd : _ t)
| "progressUpdate" -> (ProgressUpdate : _ t)
| "progressStart" -> (ProgressStart : _ t)
| "capabilities" -> (Capabilities : _ t)
| "process" -> (Process : _ t)
| "loadedSource" -> (LoadedSource : _ t)
| "module" -> (Module_ : _ t)
| "breakpoint" -> (Breakpoint : _ t)
| "output" -> (Output : _ t)
| "thread" -> (Thread : _ t)
| "terminated" -> (Terminated : _ t)
| "exited" -> (Exited : _ t)
| "continued" -> (Continued : _ t)
| "stopped" -> (Stopped : _ t)
| "initialized" -> (Initialized : _ t)| _ -> failwith "Event"


let enc = Data_encoding.conv f g Data_encoding.string

