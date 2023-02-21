module Parser = Mdb_import.Tez.Parser
module Entrypoint = Mdb_import.Tez.Ctxt.Entrypoint

(* need to retain the trace of code locs of any michelson errors *)

(* include Expr *)
type t = Parser.parsed
type entrypoint = Entrypoint.t


let of_source ?(check_micheline_indentation = true) script =
  let ast, errs =
    Parser.parse_toplevel ~check:check_micheline_indentation script
  in
  match errs with
   | [] -> Ok ast
   | lst -> Error lst


(** Parse a Michelson expression from string, raising an exception on error,
    in this version we keep hold of the inner errors. *)
let from_string ?(check_micheline_indentation = true) str =
  let ast, errs =
    Parser.parse_expression ~check:check_micheline_indentation str
  in
  match errs with
   | [] -> Ok ast
   | lst -> Error lst



module File_locations = struct
  module LocTbl = Hashtbl.Make(struct
      type t = int
      let equal i j = i=j
      let hash i = i land max_int
    end)


  let of_script (script:t) =
    script.expansion_table
    |> List.map (fun (loc_ix, (loc, _)) -> (loc_ix, loc))
    |> List.to_seq
    |> LocTbl.of_seq


  let get t loc =
    match LocTbl.find t loc with
    | Some _ as x -> x
    | None ->
      let () = Logs.err (fun m -> m "Couldnt retrieve file location for loc '%d'" loc) in
      None

  type t = Tezos_micheline.Micheline_parser.location LocTbl.t

end
