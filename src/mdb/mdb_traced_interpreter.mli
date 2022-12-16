open Mdb_types

module T : functor (CFG:INTERPRETER_CFG) -> INTERPRETER with type expansion_table = (int * (Tezos_micheline.Micheline_parser.location * int list)) array
