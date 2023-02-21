type t

val location : t -> Tezos_micheline.Micheline_parser.location
val gas : t -> string
val stack : t -> string list

val make :
  location:Tezos_micheline.Micheline_parser.location ->
  gas:string ->
  stack:string list ->
  unit ->
  t

val enc :
  t Data_encoding.t
