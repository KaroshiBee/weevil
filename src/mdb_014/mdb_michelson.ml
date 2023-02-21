module P = Mdb_import.Tez.Parser
module E = Mdb_import.Tez.Ctxt.Entrypoint
module Env = Mdb_import.Tez.Env

(* need to retain the trace of code locs of any michelson errors *)

module Script = struct
  type t = P.parsed

  let from_string ?(check_micheline_indentation = true) script =
    let (ast, errs) =
      P.parse_toplevel ~check:check_micheline_indentation script
    in
    match errs with [] -> Ok ast | lst -> Error lst

  let expanded (t:P.parsed) = t.expanded

end

module Expr = struct
  type t = P.parsed

  (** Parse a Michelson expression from string, raising an exception on error,
    in this version we keep hold of the inner errors. *)
  let from_string ?(check_micheline_indentation = true) str =
    let (ast, errs) =
      P.parse_expression ~check:check_micheline_indentation str
    in
    match errs with [] -> Ok ast | lst -> Error lst

  let expanded (t:P.parsed) = t.expanded

end

module Entrypoint = struct
  type t = E.t

  let from_string entrypoint = E.of_string_lax entrypoint |> Env.wrap_tzresult
  let to_entrypoint t = t

end

module LocTbl = Hashtbl.Make (struct
  type t = int

  let equal i j = i = j

  let hash i = i land max_int
end)

module File_locations = struct
  type locs = Tezos_micheline.Micheline_parser.location LocTbl.t

  let of_script (script : Script.t) =
    script.expansion_table
    |> List.map (fun (loc_ix, (loc, _)) -> (loc_ix, loc))
    |> List.to_seq |> LocTbl.of_seq

  let get t loc =
    match LocTbl.find t loc with
    | Some _ as x -> x
    | None ->
        let () =
          Logs.err (fun m ->
              m "Couldnt retrieve file location for loc '%d'" loc)
        in
        None
end
