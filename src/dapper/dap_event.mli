(* NOTE autogenerating Event, do not manually edit *)
type 'a t
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

val memory : memory t
val invalidated : invalidated t
val progressEnd : progressEnd t
val progressUpdate : progressUpdate t
val progressStart : progressStart t
val capabilities : capabilities t
val process : process t
val loadedSource : loadedSource t
val module_ : module_ t
val breakpoint : breakpoint t
val output : output t
val thread : thread t
val terminated : terminated t
val exited : exited t
val continued : continued t
val stopped : stopped t
val initialized : initialized t

val enc : 'a t Data_encoding.t