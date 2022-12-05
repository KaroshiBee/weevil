
type t

val make :
    ?backend_ip:string ->
    ?backend_port:int ->
    ?backend_cmd:string ->
    ?stepper_cmd:(script_filename:string -> storage:string -> parameter:string -> string) ->
    unit ->
    t

val make_address : string -> Unix.inet_addr

val backend_port : t -> int

val backend_ip : t -> Unix.inet_addr

val backend_cmd : t -> string

val to_process_command : string -> string * string array

val stepper_cmd : script_filename:string -> storage:string -> parameter:string -> t -> string
