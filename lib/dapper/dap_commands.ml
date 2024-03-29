(* NOTE autogenerated, do not manually edit *)
 type v = Disassemble | WriteMemory | ReadMemory | ExceptionInfo | Completions | GotoTargets | StepInTargets | SetExpression | Evaluate | LoadedSources | Modules | TerminateThreads | Threads | Source | SetVariable | Variables | Scopes | StackTrace | Pause | Goto | RestartFrame | ReverseContinue | StepBack | StepOut | StepIn | Next | Continue | SetInstructionBreakpoints | SetDataBreakpoints | DataBreakpointInfo | SetExceptionBreakpoints | SetFunctionBreakpoints | SetBreakpoints | BreakpointLocations | Terminate | Disconnect | Restart | Attach | Launch | ConfigurationDone | Initialize | RunInTerminal | Cancel | Error [@@deriving eq]

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

let equal = equal_v

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

let to_string (type a) : a t -> string = function | (Disassemble : disassemble t) -> "disassemble"
| (WriteMemory : writeMemory t) -> "writeMemory"
| (ReadMemory : readMemory t) -> "readMemory"
| (ExceptionInfo : exceptionInfo t) -> "exceptionInfo"
| (Completions : completions t) -> "completions"
| (GotoTargets : gotoTargets t) -> "gotoTargets"
| (StepInTargets : stepInTargets t) -> "stepInTargets"
| (SetExpression : setExpression t) -> "setExpression"
| (Evaluate : evaluate t) -> "evaluate"
| (LoadedSources : loadedSources t) -> "loadedSources"
| (Modules : modules t) -> "modules"
| (TerminateThreads : terminateThreads t) -> "terminateThreads"
| (Threads : threads t) -> "threads"
| (Source : source t) -> "source"
| (SetVariable : setVariable t) -> "setVariable"
| (Variables : variables t) -> "variables"
| (Scopes : scopes t) -> "scopes"
| (StackTrace : stackTrace t) -> "stackTrace"
| (Pause : pause t) -> "pause"
| (Goto : goto t) -> "goto"
| (RestartFrame : restartFrame t) -> "restartFrame"
| (ReverseContinue : reverseContinue t) -> "reverseContinue"
| (StepBack : stepBack t) -> "stepBack"
| (StepOut : stepOut t) -> "stepOut"
| (StepIn : stepIn t) -> "stepIn"
| (Next : next t) -> "next"
| (Continue : continue t) -> "continue"
| (SetInstructionBreakpoints : setInstructionBreakpoints t) -> "setInstructionBreakpoints"
| (SetDataBreakpoints : setDataBreakpoints t) -> "setDataBreakpoints"
| (DataBreakpointInfo : dataBreakpointInfo t) -> "dataBreakpointInfo"
| (SetExceptionBreakpoints : setExceptionBreakpoints t) -> "setExceptionBreakpoints"
| (SetFunctionBreakpoints : setFunctionBreakpoints t) -> "setFunctionBreakpoints"
| (SetBreakpoints : setBreakpoints t) -> "setBreakpoints"
| (BreakpointLocations : breakpointLocations t) -> "breakpointLocations"
| (Terminate : terminate t) -> "terminate"
| (Disconnect : disconnect t) -> "disconnect"
| (Restart : restart t) -> "restart"
| (Attach : attach t) -> "attach"
| (Launch : launch t) -> "launch"
| (ConfigurationDone : configurationDone t) -> "configurationDone"
| (Initialize : initialize t) -> "initialize"
| (RunInTerminal : runInTerminal t) -> "runInTerminal"
| (Cancel : cancel t) -> "cancel"
| (Error : error t) -> "error"

let from_string (type a) : string -> a t = function | "disassemble" -> (Disassemble : disassemble t)
| "writeMemory" -> (WriteMemory : writeMemory t)
| "readMemory" -> (ReadMemory : readMemory t)
| "exceptionInfo" -> (ExceptionInfo : exceptionInfo t)
| "completions" -> (Completions : completions t)
| "gotoTargets" -> (GotoTargets : gotoTargets t)
| "stepInTargets" -> (StepInTargets : stepInTargets t)
| "setExpression" -> (SetExpression : setExpression t)
| "evaluate" -> (Evaluate : evaluate t)
| "loadedSources" -> (LoadedSources : loadedSources t)
| "modules" -> (Modules : modules t)
| "terminateThreads" -> (TerminateThreads : terminateThreads t)
| "threads" -> (Threads : threads t)
| "source" -> (Source : source t)
| "setVariable" -> (SetVariable : setVariable t)
| "variables" -> (Variables : variables t)
| "scopes" -> (Scopes : scopes t)
| "stackTrace" -> (StackTrace : stackTrace t)
| "pause" -> (Pause : pause t)
| "goto" -> (Goto : goto t)
| "restartFrame" -> (RestartFrame : restartFrame t)
| "reverseContinue" -> (ReverseContinue : reverseContinue t)
| "stepBack" -> (StepBack : stepBack t)
| "stepOut" -> (StepOut : stepOut t)
| "stepIn" -> (StepIn : stepIn t)
| "next" -> (Next : next t)
| "continue" -> (Continue : continue t)
| "setInstructionBreakpoints" -> (SetInstructionBreakpoints : setInstructionBreakpoints t)
| "setDataBreakpoints" -> (SetDataBreakpoints : setDataBreakpoints t)
| "dataBreakpointInfo" -> (DataBreakpointInfo : dataBreakpointInfo t)
| "setExceptionBreakpoints" -> (SetExceptionBreakpoints : setExceptionBreakpoints t)
| "setFunctionBreakpoints" -> (SetFunctionBreakpoints : setFunctionBreakpoints t)
| "setBreakpoints" -> (SetBreakpoints : setBreakpoints t)
| "breakpointLocations" -> (BreakpointLocations : breakpointLocations t)
| "terminate" -> (Terminate : terminate t)
| "disconnect" -> (Disconnect : disconnect t)
| "restart" -> (Restart : restart t)
| "attach" -> (Attach : attach t)
| "launch" -> (Launch : launch t)
| "configurationDone" -> (ConfigurationDone : configurationDone t)
| "initialize" -> (Initialize : initialize t)
| "runInTerminal" -> (RunInTerminal : runInTerminal t)
| "cancel" -> (Cancel : cancel t)
| "error" -> (Error : error t)| _ -> failwith "Dap_commands"



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
      