open Mdb_types

module T : functor (CFG:INTERPRETER_CFG) -> INTERPRETER
  with type log_records = CFG.t Mdb_log_records.t
