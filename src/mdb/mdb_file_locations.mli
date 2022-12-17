type t

val of_script : Mdb_typechecker.t -> t

val get : t -> int -> Tezos_micheline.Micheline_parser.location option
