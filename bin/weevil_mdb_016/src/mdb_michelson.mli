module Script : sig
  type t
  val from_string : ?check_micheline_indentation:bool -> string -> t tzresult
  val expanded : t -> Mdb_import.Tez.Prot.Michelson_v1_primitives.prim Tezos_micheline.Micheline.canonical
end

module Expr : sig
  type t
  val from_string : ?check_micheline_indentation:bool -> string -> t tzresult
  val expanded : t -> Mdb_import.Tez.Prot.Michelson_v1_primitives.prim Tezos_micheline.Micheline.canonical
end

module Entrypoint : sig
  type t
  val from_string : string -> t tzresult
  val to_entrypoint : t -> Mdb_import.Tez.Ctxt.Entrypoint.t
end

module File_locations : sig
  type locs
  val of_script : Script.t -> locs
  val get : locs -> int -> Tezos_micheline.Micheline_parser.location option
end
