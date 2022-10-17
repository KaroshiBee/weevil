(* NOTE autogenerating Command, do not manually edit *)
type 'a t
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

val disassemble : disassemble t
val writeMemory : writeMemory t
val readMemory : readMemory t
val exceptionInfo : exceptionInfo t
val completions : completions t
val gotoTargets : gotoTargets t
val stepInTargets : stepInTargets t
val setExpression : setExpression t
val evaluate : evaluate t
val loadedSources : loadedSources t
val modules : modules t
val terminateThreads : terminateThreads t
val threads : threads t
val source : source t
val setVariable : setVariable t
val variables : variables t
val scopes : scopes t
val stackTrace : stackTrace t
val pause : pause t
val goto : goto t
val restartFrame : restartFrame t
val reverseContinue : reverseContinue t
val stepBack : stepBack t
val stepOut : stepOut t
val stepIn : stepIn t
val next : next t
val continue : continue t
val setInstructionBreakpoints : setInstructionBreakpoints t
val setDataBreakpoints : setDataBreakpoints t
val dataBreakpointInfo : dataBreakpointInfo t
val setExceptionBreakpoints : setExceptionBreakpoints t
val setFunctionBreakpoints : setFunctionBreakpoints t
val setBreakpoints : setBreakpoints t
val breakpointLocations : breakpointLocations t
val terminate : terminate t
val disconnect : disconnect t
val restart : restart t
val attach : attach t
val launch : launch t
val configurationDone : configurationDone t
val initialize : initialize t
val runInTerminal : runInTerminal t
val cancel : cancel t
val error : error t

val enc : 'a t Data_encoding.t