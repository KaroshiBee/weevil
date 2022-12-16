(* NOTE basically is a sort of flexible sized array
   also maintain the notion of old and new records so that the front end
   can show only the new ones
*)

type 'log_element t

val make : unit -> 'log_element t

val remove_all : 'log_element t -> unit

val add_new : 'log_element t -> 'log_element -> unit

val add_old : 'log_element t -> 'log_element -> unit

val mem : 'log_element t -> int -> bool

val to_list : 'log_element t -> 'log_element list

val new_to_old_inplace :
  keep_old:bool -> 'log_element t -> unit
