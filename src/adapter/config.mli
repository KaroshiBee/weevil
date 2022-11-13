
type t

val make :
    ?backend_ip:string ->
    ?backend_port:int ->
    ?backend_cmd:string ->
    ?stepper_cmd:string ->
    ?backend_echo:string ->
    unit ->
    t

val make_address : string -> Unix.inet_addr

val backend_port : t -> int

val backend_ip : t -> Unix.inet_addr

val backend_cmd : t -> string

val to_process_command : string -> string * string array

val stepper_cmd : t -> string
