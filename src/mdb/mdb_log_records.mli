open Protocol
open Alpha_context


type 'log_element t

val make : unit -> 'log_element t
val add_new : 'log_element t -> Script.location -> 'log_element -> unit
val add_old : 'log_element t -> Script.location -> 'log_element -> unit
val mem : 'log_element t -> Script.location -> bool
val to_list : 'log_element t -> 'log_element list
val new_to_old_inplace :
  keep_old:bool -> 'log_element t -> unit
