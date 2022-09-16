module Sp = Dap_specs
module Q = Json_query
module S = Json_schema

module Visited = struct

  type status = | Started | Finished | Unknown [@@deriving show]

  type t = (string, status) Hashtbl.t

  let pp ppf values =
    Hashtbl.iter (fun ky vl ->
        Format.fprintf ppf "@[<1>%s: %s@]@." ky (show_status vl)
          )
      values
end


module Dfs = struct
  let _CMD = "/definitions/FAKE/command" (* fake Command enum path *)

  let _EV = "/definitions/FAKE/event" (* fake Event enum path *)

  let _MSG = "/definitions/FAKE/protocol_message_type"


  type t = {
    nodes: Sp.t list;
    visited: Visited.t;
    finished: Sp.t list;
  } [@@deriving show]

  let _set_field_type t type_ =
      match t.nodes with
      | Sp.Field field_spec :: nodes ->
        let enc_ = type_ in
        let field = Sp.Field_spec.{field_spec with type_; enc_} in
        let nodes = (Sp.Field field) :: nodes in
        {t with nodes}
      | _ -> t


  let rec process_definition t ~schema_js ~path =
    (* this will be a *Request/*Response/*Event/Object top level definition
       will want to ignore base class ProtocolMessage, Request, Response, Event types
       as they are handled with the functors in Dap_t
       only want to recurse down into the defn if it is not already being/been visited
       and, if so, we want to recurse after setting the initial defn in elements map
       with the notion that some objects might get rewritten as enums
    *)
    let dfn = Q.json_pointer_of_path path in

    Logs.debug (fun m -> m "process dfn start: '%s'" dfn) ;
    let t =
      if Sp.is_special_definition ~path then (
        Logs.debug (fun m -> m "special case: '%s'" dfn) ;
        t)
      else
        (* check is valid name by finding the definition *)
        let schema = Json_schema.of_json schema_js in
        let element = S.find_definition dfn schema in
        (* if added new one then recurse too *)
        let has_visited = Hashtbl.mem t.visited dfn in
        if (not has_visited) then (
          Logs.debug (fun m -> m "visiting: '%s'" dfn) ;
          Hashtbl.add t.visited dfn Started;
          let t = process_element t ~schema_js ~path element in
          Hashtbl.replace t.visited dfn Finished;
          Logs.debug (fun m -> m "finished: '%s'" dfn) ;
          t
        )
        else (
          Logs.debug (fun m -> m "already visiting/visited: '%s'" dfn) ;
          t
        )
    in
    Logs.debug (fun m -> m "process dfn end: '%s'" dfn) ;
    t

  and process_element t ~schema_js ~path el =
    (* TODO can get proper enums here, still need to qry for _enums *)
    Logs.debug (fun m ->
        m
          "process element under '%s'"
          (Q.json_pointer_of_path ~wildcards:true path)) ;
    process_kind t ~schema_js ~path el.kind


  and process_kind t ~schema_js ~path = function
    | Object o when (List.length o.properties) = 0 ->
        Logs.debug (fun m ->
            m
              "process object with 0 properties under '%s'"
              (Q.json_pointer_of_path ~wildcards:true path)) ;
        (* TODO things like Message.variables have additionalProperties *)
        _set_field_type t "empty_object"

    | Object
        {
          properties;
          pattern_properties;
          additional_properties;
          min_properties;
          max_properties;
          schema_dependencies;
          property_dependencies;
        } -> (
        assert (0 = List.length pattern_properties) ;
        assert (0 = List.length schema_dependencies) ;
        assert (0 = List.length property_dependencies) ;
        assert (0 = min_properties) ;
        assert (Option.is_none max_properties) ;
        assert (Option.is_some additional_properties) ;
        let n = List.length properties in
        let m = List.length t.nodes in
        let spec = Sp.make ~path () in
        let nodes = spec :: t.nodes in
        Logs.debug (fun m -> m "got spec for %s with %s" (Q.json_pointer_of_path path) (Sp.show spec)) ;
        Logs.debug (fun m ->
            m
              "process object with %d properties under '%s'"
              n (Q.json_pointer_of_path ~wildcards:true path)) ;
        let t = properties
                |> List.fold_left
                  (fun t_acc (pname, ty, required, _extra) ->
                     Logs.debug (fun m ->
                         m
                           "process property '%s' under '%s'"
                           pname
                           (Q.json_pointer_of_path ~wildcards:true path)) ;
                     let new_path =
                       path @ [`Field "properties"; `Field pname]
                     in
                     let new_spec = Sp.Field (Sp.Field_spec.make ~dirty_name:pname ~required ()) in
                     let new_nodes =  new_spec :: t_acc.nodes in
                     let t' = process_element {t_acc with nodes=new_nodes} ~schema_js ~path:new_path ty in
                     Hashtbl.replace_seq t_acc.visited (Hashtbl.to_seq t'.visited) ;
                     {t' with visited = t_acc.visited}
                  ) {t with nodes} in
        assert (List.length t.nodes >= m+n) ;
        (* now pop off the n-top of the node list and make the object spec *)
        let arr = Array.of_list t.nodes in
        let fields = Array.sub arr 0 n |> Array.to_list |> List.map (fun f -> match f with | Sp.Field spec -> spec | s ->  failwith (Printf.sprintf "Expected field, got %s" (Sp.show s))) in
        let nodes = Array.sub arr n (Array.length arr - n) |> Array.to_list in
        match nodes with
        | Sp.Object spec :: nodes ->
          let finished = Sp.(Object Obj_spec.{spec with fields} ) :: t.finished in
          {t with nodes; finished}
        | _ -> t
      )
    | Array (_, _) -> failwith "TODO array"
    | Monomorphic_array _ -> failwith "TODO marray"
        (* (element, {min_items; max_items; unique_items; additional_items}) -> *)
        (* assert (0 = min_items) ; *)
        (* assert (Option.is_none max_items) ; *)
        (* assert (not unique_items) ; *)
        (* assert (Option.is_none additional_items) ; *)
        (* Logs.debug (fun m -> *)
        (*     m *)
        (*       "process mono-morphic array under '%s'" *)
        (*       (Q.json_pointer_of_path ~wildcards:true path)) ; *)
        (* let new_path = path @ [`Field "items"] in *)
        (* let t = process_element t ~schema_js ~path:new_path element in *)
        (* (\* TODO should now be able to make the array specs *\) *)
        (* (\* let field_name = ModuleName.of_path ~path in *)
        (*  * let inner_type = ModuleName.of_path ~path:new_path in *)
        (*  * let leaf = LeafNodes.(`Array {field_name; inner_type}) in *)
        (*  * add_leaf_node t ~path ~leaf; *\) *)
        (* let names = *)
        (*   Names.add_sorted_module_name t.names ~path ~desc:"array items" *)
        (* in *)
        (* {t with names} *)
    | Combine (c, elements) -> (
        match c with
        | All_of ->
            Logs.debug (fun m ->
                m
                  "process combination allOf with %d elements under '%s'"
                  (List.length elements)
                  (Q.json_pointer_of_path ~wildcards:true path)) ;
            let (_, t) =
              elements
              |> List.fold_left
                   (fun (i, t_acc) el ->
                     let new_path = path @ [`Field "allOf"; `Index i] in
                     let t' = process_element t_acc ~schema_js ~path:new_path el in
                     Hashtbl.replace_seq t_acc.visited (Hashtbl.to_seq t'.visited) ;
                     (i + 1, {t' with visited = t_acc.visited}))
                   (0, t)
            in

            (* (\* make an all_of thingy after filter out Request/Response/Event/ProtocolMessage refs  *\)
             * let nts =
             *   elements
             *   |> List.mapi (fun i _el ->
             *       let new_path = path @ [`Field "allOf"; `Index i] in
             *       let field_name = ModuleName.of_path ~path in
             *       let field_type = ModuleName.of_path ~path:new_path in
             *       ({field_name; field_type} : LeafNodes.name_type_specs)
             *     )
             * in
             * let leaf, desc =
             *   try
             *     let leaf_spec = EventSpec.of_path_exn ~path in
             *     let leaf = `Event leaf_spec in
             *     (leaf, "event")
             *   with _ ->
             *     try
             *       let leaf_spec = ResponseSpec.of_path_exn ~path in
             *       let leaf = `Response leaf_spec in
             *       (leaf, "response")
             *     with _ ->
             *       try
             *         let leaf_spec = RequestSpec.of_path_exn ~path in
             *         let leaf = `Request leaf_spec in
             *         (leaf, "request")
             *       with _ ->
             *         `AllOf nts, "allof"
             * in
             *   add_leaf_node t ~path ~leaf; *)
            t
        | One_of -> failwith "TODO OneOf"
        | Any_of -> failwith "TODO AnyOf"
        | Not ->
            failwith
              (Printf.sprintf
                 "TODO combinator NOT @ %s"
                 (Q.json_pointer_of_path ~wildcards:true path)))
    | Def_ref ref_path ->
        let ref_path_str = Q.json_pointer_of_path ref_path in
        Logs.debug (fun m ->
            m
              "Dependencies - def_ref: path '%s', ref_path: '%s'"
              (Q.json_pointer_of_path path)
              ref_path_str) ;
        let is_cyclic = false
        in

        let t =
          if is_cyclic then t
          else
            let t' = process_definition t ~schema_js ~path:ref_path in
            _set_field_type t' "Message.t"
        in
        t

    | Id_ref _ -> failwith "TODO Id_ref"
    | Ext_ref _ -> failwith "TODO Ext_ref"
    | String _ ->
      Logs.debug (fun m -> m "String");
      _set_field_type t "string"
    | Integer _ ->
      Logs.debug (fun m -> m "Int");
      _set_field_type t "int"
    | Number _ ->
      Logs.debug (fun m -> m "Number");
      _set_field_type t "int"
    | Boolean ->
      Logs.debug (fun m -> m "Bool");
      _set_field_type t "bool"
    | Null -> failwith "TODO Null"
    | Any -> failwith "TODO Any"
    | Dummy -> failwith "TODO Dummy"

  let make ~schema_js =
    let names = function
      | `O fields -> fields |> List.map (fun (nm, _) -> nm)
      | _ -> []
    in
    let empty = {nodes = []; visited = Hashtbl.create 500; finished = []} in
    (* NOTE know all /definitions/ are objects and are the only top-level things *)
    let ns = Q.query [`Field "definitions"] schema_js |> names in
    Logs.info (fun m -> m "\n\nprocessing '%d' names" @@ List.length ns) ;
    ns
    |> List.fold_left
         (fun t_acc nm ->
           let path = [`Field "definitions"; `Field nm] in
           let t = process_definition t_acc ~schema_js ~path in
           Hashtbl.replace_seq t_acc.visited (Hashtbl.to_seq t.visited) ;
           {t with visited = t_acc.visited})
         empty

  (* let process t = *)
  (*   let wksp : (string * Sp.Obj_spec.t) list ref = ref [] in *)
  (*   let rec aux = function *)
  (*     | Leaf (Value leaf_specs) :: Prop (Object prop_specs) :: tl -> *)
  (*       Logs.debug (fun m -> m "\n\nprocessing leaf '%s' and prop '%s'" (Sp.show_value leaf_specs) (Sp.Obj_spec.show prop_specs)) ; *)
  (*       wksp := (Sp.show_value leaf_specs, prop_specs) :: !wksp; *)
  (*       aux tl *)
  (*     | Leaf (Object leaf_specs) :: Prop (Object prop_specs) :: tl -> *)
  (*       Logs.debug (fun m -> m "\n\nprocessing leaf '%s' and prop '%s'" (Sp.Obj_spec.show leaf_specs) (Sp.Obj_spec.show prop_specs)) ; *)
  (*       wksp := ("any", prop_specs) :: !wksp; *)
  (*       aux tl *)
  (*     | Node (Sp.Object specs) :: tl -> *)
  (*       Logs.debug (fun m -> m "\n\nprocessing '%s'" @@ Sp.Obj_spec.show specs) ; *)
  (*       let fields = !wksp *)
  (*           |> List.map (fun (leaf, prop) -> *)
  (*                        let pth = List.rev Sp.Obj_spec.(prop.path) in *)
  (*                        let dirty_name = match List.hd pth with `Field nm -> nm | _ -> "unknown" in *)
  (*                        let safe_name = Sp._unweird_name dirty_name in *)
  (*                        let type_ = leaf in *)
  (*                        let enc_ = type_ in *)
  (*                        let required = true in *)
  (*                        Sp.Obj_spec.{dirty_name; safe_name; type_; enc_; required} *)
  (*                      ) in *)
  (*       wksp := []; *)
  (*       {specs with fields} :: aux tl *)

  (*     | _ -> [] *)
  (*   in *)
  (*   let nodes = aux t.nodes in *)
  (*   nodes *)


end
