
module LocTbl = Hashtbl.Make(struct
    type t = int
    let equal i j = i=j
    let hash i = i land max_int
  end)


type t = Tezos_micheline.Micheline_parser.location LocTbl.t

let of_script (script:Mdb_typechecker.t) =
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
