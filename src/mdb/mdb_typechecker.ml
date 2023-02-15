module Michelson_v1_parser = Mdb_tezos.Tez.Parser

(* need to retain the trace of code locs of any michelson errors *)

(* include Expr *)
type t = Michelson_v1_parser.parsed

let of_source ?(check_micheline_indentation = true) script =
  let ast, errs =
    Michelson_v1_parser.parse_toplevel ~check:check_micheline_indentation script
  in
  match errs with
   | [] -> Ok ast
   | lst -> Error lst


(** Parse a Michelson expression from string, raising an exception on error,
    in this version we keep hold of the inner errors. *)
let from_string ?(check_micheline_indentation = true) str =
  let ast, errs =
    Michelson_v1_parser.parse_expression ~check:check_micheline_indentation str
  in
  match errs with
   | [] -> Ok ast
   | lst -> Error lst
