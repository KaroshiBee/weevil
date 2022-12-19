type t

(* we use the expansion table from Tezos_micheline.Micheline_parser *)
val of_script : Mdb_typechecker.t -> t

val get : t -> int -> Tezos_micheline.Micheline_parser.location option
