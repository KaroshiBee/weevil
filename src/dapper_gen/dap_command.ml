(* NOTE autogenerating Command, do not manually edit *)
type v = Disassemble | WriteMemory | ReadMemory | ExceptionInfo | Completions | GotoTargets | StepInTargets | SetExpression | Evaluate | LoadedSources | Modules | TerminateThreads | Threads | Source | SetVariable | Variables | Scopes | StackTrace | Pause | Goto | RestartFrame | ReverseContinue | StepBack | StepOut | StepIn | Next | Continue | SetInstructionBreakpoints | SetDataBreakpoints | DataBreakpointInfo | SetExceptionBreakpoints | SetFunctionBreakpoints | SetBreakpoints | BreakpointLocations | Terminate | Disconnect | Restart | Attach | Launch | ConfigurationDone | Initialize | RunInTerminal | Cancel | Error

type 'a t = v

type disassemble
type writeMemory
type readMemory
type exceptionInfo
type completions
type gotoTargets
type stepInTargets
type setExpression
type evaluate
type loadedSources
type modules
type terminateThreads
type threads
type source
type setVariable
type variables
type scopes
type stackTrace
type pause
type goto
type restartFrame
type reverseContinue
type stepBack
type stepOut
type stepIn
type next
type continue
type setInstructionBreakpoints
type setDataBreakpoints
type dataBreakpointInfo
type setExceptionBreakpoints
type setFunctionBreakpoints
type setBreakpoints
type breakpointLocations
type terminate
type disconnect
type restart
type attach
type launch
type configurationDone
type initialize
type runInTerminal
type cancel
type error

let disassemble : disassemble t = Disassemble
let writeMemory : writeMemory t = WriteMemory
let readMemory : readMemory t = ReadMemory
let exceptionInfo : exceptionInfo t = ExceptionInfo
let completions : completions t = Completions
let gotoTargets : gotoTargets t = GotoTargets
let stepInTargets : stepInTargets t = StepInTargets
let setExpression : setExpression t = SetExpression
let evaluate : evaluate t = Evaluate
let loadedSources : loadedSources t = LoadedSources
let modules : modules t = Modules
let terminateThreads : terminateThreads t = TerminateThreads
let threads : threads t = Threads
let source : source t = Source
let setVariable : setVariable t = SetVariable
let variables : variables t = Variables
let scopes : scopes t = Scopes
let stackTrace : stackTrace t = StackTrace
let pause : pause t = Pause
let goto : goto t = Goto
let restartFrame : restartFrame t = RestartFrame
let reverseContinue : reverseContinue t = ReverseContinue
let stepBack : stepBack t = StepBack
let stepOut : stepOut t = StepOut
let stepIn : stepIn t = StepIn
let next : next t = Next
let continue : continue t = Continue
let setInstructionBreakpoints : setInstructionBreakpoints t = SetInstructionBreakpoints
let setDataBreakpoints : setDataBreakpoints t = SetDataBreakpoints
let dataBreakpointInfo : dataBreakpointInfo t = DataBreakpointInfo
let setExceptionBreakpoints : setExceptionBreakpoints t = SetExceptionBreakpoints
let setFunctionBreakpoints : setFunctionBreakpoints t = SetFunctionBreakpoints
let setBreakpoints : setBreakpoints t = SetBreakpoints
let breakpointLocations : breakpointLocations t = BreakpointLocations
let terminate : terminate t = Terminate
let disconnect : disconnect t = Disconnect
let restart : restart t = Restart
let attach : attach t = Attach
let launch : launch t = Launch
let configurationDone : configurationDone t = ConfigurationDone
let initialize : initialize t = Initialize
let runInTerminal : runInTerminal t = RunInTerminal
let cancel : cancel t = Cancel
let error : error t = Error

let f : type a. a t -> string = function | (Disassemble : _ t) -> "disassemble"
| (WriteMemory : _ t) -> "writeMemory"
| (ReadMemory : _ t) -> "readMemory"
| (ExceptionInfo : _ t) -> "exceptionInfo"
| (Completions : _ t) -> "completions"
| (GotoTargets : _ t) -> "gotoTargets"
| (StepInTargets : _ t) -> "stepInTargets"
| (SetExpression : _ t) -> "setExpression"
| (Evaluate : _ t) -> "evaluate"
| (LoadedSources : _ t) -> "loadedSources"
| (Modules : _ t) -> "modules"
| (TerminateThreads : _ t) -> "terminateThreads"
| (Threads : _ t) -> "threads"
| (Source : _ t) -> "source"
| (SetVariable : _ t) -> "setVariable"
| (Variables : _ t) -> "variables"
| (Scopes : _ t) -> "scopes"
| (StackTrace : _ t) -> "stackTrace"
| (Pause : _ t) -> "pause"
| (Goto : _ t) -> "goto"
| (RestartFrame : _ t) -> "restartFrame"
| (ReverseContinue : _ t) -> "reverseContinue"
| (StepBack : _ t) -> "stepBack"
| (StepOut : _ t) -> "stepOut"
| (StepIn : _ t) -> "stepIn"
| (Next : _ t) -> "next"
| (Continue : _ t) -> "continue"
| (SetInstructionBreakpoints : _ t) -> "setInstructionBreakpoints"
| (SetDataBreakpoints : _ t) -> "setDataBreakpoints"
| (DataBreakpointInfo : _ t) -> "dataBreakpointInfo"
| (SetExceptionBreakpoints : _ t) -> "setExceptionBreakpoints"
| (SetFunctionBreakpoints : _ t) -> "setFunctionBreakpoints"
| (SetBreakpoints : _ t) -> "setBreakpoints"
| (BreakpointLocations : _ t) -> "breakpointLocations"
| (Terminate : _ t) -> "terminate"
| (Disconnect : _ t) -> "disconnect"
| (Restart : _ t) -> "restart"
| (Attach : _ t) -> "attach"
| (Launch : _ t) -> "launch"
| (ConfigurationDone : _ t) -> "configurationDone"
| (Initialize : _ t) -> "initialize"
| (RunInTerminal : _ t) -> "runInTerminal"
| (Cancel : _ t) -> "cancel"
| (Error : _ t) -> "error"

let g : type a. string -> a t = function | "disassemble" -> (Disassemble : _ t)
| "writeMemory" -> (WriteMemory : _ t)
| "readMemory" -> (ReadMemory : _ t)
| "exceptionInfo" -> (ExceptionInfo : _ t)
| "completions" -> (Completions : _ t)
| "gotoTargets" -> (GotoTargets : _ t)
| "stepInTargets" -> (StepInTargets : _ t)
| "setExpression" -> (SetExpression : _ t)
| "evaluate" -> (Evaluate : _ t)
| "loadedSources" -> (LoadedSources : _ t)
| "modules" -> (Modules : _ t)
| "terminateThreads" -> (TerminateThreads : _ t)
| "threads" -> (Threads : _ t)
| "source" -> (Source : _ t)
| "setVariable" -> (SetVariable : _ t)
| "variables" -> (Variables : _ t)
| "scopes" -> (Scopes : _ t)
| "stackTrace" -> (StackTrace : _ t)
| "pause" -> (Pause : _ t)
| "goto" -> (Goto : _ t)
| "restartFrame" -> (RestartFrame : _ t)
| "reverseContinue" -> (ReverseContinue : _ t)
| "stepBack" -> (StepBack : _ t)
| "stepOut" -> (StepOut : _ t)
| "stepIn" -> (StepIn : _ t)
| "next" -> (Next : _ t)
| "continue" -> (Continue : _ t)
| "setInstructionBreakpoints" -> (SetInstructionBreakpoints : _ t)
| "setDataBreakpoints" -> (SetDataBreakpoints : _ t)
| "dataBreakpointInfo" -> (DataBreakpointInfo : _ t)
| "setExceptionBreakpoints" -> (SetExceptionBreakpoints : _ t)
| "setFunctionBreakpoints" -> (SetFunctionBreakpoints : _ t)
| "setBreakpoints" -> (SetBreakpoints : _ t)
| "breakpointLocations" -> (BreakpointLocations : _ t)
| "terminate" -> (Terminate : _ t)
| "disconnect" -> (Disconnect : _ t)
| "restart" -> (Restart : _ t)
| "attach" -> (Attach : _ t)
| "launch" -> (Launch : _ t)
| "configurationDone" -> (ConfigurationDone : _ t)
| "initialize" -> (Initialize : _ t)
| "runInTerminal" -> (RunInTerminal : _ t)
| "cancel" -> (Cancel : _ t)
| "error" -> (Error : _ t)| _ -> failwith "Command"


let enc = Data_encoding.conv f g Data_encoding.string

