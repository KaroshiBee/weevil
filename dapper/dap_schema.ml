module El = Dap_again
module Q = Json_query
module S = Json_schema

module Dfs = struct

  module Data = Hashtbl.Make(struct type t = string let equal = String.equal let hash = Hashtbl.hash end)

  (* let _KEY = "names"
   * let _VISITED = "visited"
   * let _EL = "elements" *)

  type visit_status = Started | Finished | Unknown

  type t = {
    names: (Q.path*string) list;
    visited: visit_status Data.t;
  }

  let add_sorted_module_name t ~path ~desc =
    {t with names=(path, desc)::t.names}

  let check_and_add_visited t ~path =
    (* add to the visited tbl if not already there,
       returns whether a new one was added,
       dfn that result in a Command or Event get added under their full path *)
    let dfn = Q.json_pointer_of_path path in
    let is_present = Data.mem t.visited dfn in
    if not is_present then
      Data.replace t.visited dfn Started;
    t, (not is_present)

  let assert_and_close_visited t ~path =
    (* asserts is in the visited tbl
       and then sets its visit_status to Finished *)
    let dfn = Q.json_pointer_of_path path in
    assert (Data.mem t.visited dfn);
    Data.replace t.visited dfn Finished;
    t

  let get_visit_status t ~path =
    let dfn = Q.json_pointer_of_path path in
    Data.find_opt t.visited dfn |> Option.value ~default:Unknown

  let rec process_definition t ~schema_js ~path =
    (* this will be a *Request/*Response/*Event/Object top level definition
       will want to ignore base class ProtocolMessage, Request, Response, Event types
       as they are handled with the functors above
       only want to recurse down into the defn if it is not already being/been visited *)
    let dfn = Q.json_pointer_of_path path in

    Logs.debug (fun m -> m "process dfn start: '%s'"  dfn);
    let t' =
      if El.is_special_definition ~path then (
        Logs.debug (fun m -> m "!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!special case: '%s'"  dfn);
        t
      )
      else (
        (* check is valid name by finding the definition *)
        let schema = Json_schema.of_json schema_js in
        let element = S.find_definition dfn schema in
        (* if added new one then recurse too *)
        let t, did_add = check_and_add_visited t ~path in
        if did_add then (
          Logs.debug (fun m -> m "visiting: '%s'"  dfn);
          let t = process_element t ~schema_js ~path element in
          let t = assert_and_close_visited t ~path in
          Logs.debug (fun m -> m "finished: '%s'"  dfn);
          t
        ) else (
          Logs.debug (fun m -> m "already visiting/visited: '%s'"  dfn);
          t
        )
      ) in
    Logs.debug (fun m -> m "process dfn end: '%s'"  dfn);
    t'


  and process_element t ~schema_js ~path el =
    (* TODO can get proper enums here, still need to qry for _enums *)
    Logs.debug (fun m -> m "process element under '%s'"  (Q.json_pointer_of_path ~wildcards:true path));
    let enums = el.enum |> Option.value ~default:[] in
    let enums = enums |> List.map Json_repr.from_any |> List.map Ezjsonm.decode_string_exn in
    let n = List.length enums in
    if n > 0 then (
      Logs.debug (fun m -> m "element under '%s' has enum [%s]"
                     (Q.json_pointer_of_path ~wildcards:true path)
                     (enums |> String.concat ", "));
      (* let field_name = ModuleName.of_path ~path in
       * let enums = enums |> List.map (fun field_name -> Enum_spec.make ~field_name ()) in *)
      (* if ModuleName.is_command field_name then (
       *   append_to_command_enum_node t ~enums
       *   (\* NOTE dont add to topo sort list *\)
       * )
       * else if ModuleName.is_event field_name then (
       *   append_to_event_enum_node t ~enums
       *   (\* NOTE dont add to topo sort list *\)
       * ) *)
      if n > 1 then (
        (* let leaf = LeafNodes.(`Enum {field_name; enums}) in *)
        (* add_leaf_node t ~path ~leaf; *)
        add_sorted_module_name t ~path ~desc:"enum"
      )
      else (
        Logs.debug (fun m -> m "Ignoring element under '%s'"
                       (Q.json_pointer_of_path ~wildcards:true path));
        t
      )
    ) else (
      process_kind t ~schema_js ~path el.kind
    )

  and process_kind t ~schema_js ~path = function
    | Object {properties; pattern_properties; additional_properties; min_properties; max_properties; schema_dependencies; property_dependencies} -> (
        assert (0 = List.length pattern_properties);
        assert (0 = List.length schema_dependencies);
        assert (0 = List.length property_dependencies);
        assert (0 = min_properties);
        assert (Option.is_none max_properties);
        assert (Option.is_some additional_properties);
        Logs.debug (fun m -> m "process object with %d properties under '%s'"  (List.length properties) (Q.json_pointer_of_path ~wildcards:true path));
        let n = List.length properties in
        if n = 0 then
          (* TODO append EmptyObject, dont need to call process_property  *)
          (* let leaf = LeafNodes.(`EmptyObject {field_name=ModuleName.of_path ~path}) in
           * add_leaf_node t ~path ~leaf; *)
          add_sorted_module_name t ~path ~desc:"empty obj"
        else (
          let t' =
            properties
            |> List.fold_left (fun acc (pname, ty, _required, _extra) ->
                Logs.debug (fun m -> m "process property '%s' under '%s'"  pname (Q.json_pointer_of_path ~wildcards:true path));
                let new_path = path @ [`Field "properties"; `Field pname ] in
                let t' = process_element acc ~schema_js ~path:new_path ty in
                (* Logs.debug (fun m -> m "PROPS: got new names [%s], replacing old names [%s]"
                 *               (t'.names |> List.map (fun (p, _) -> Q.json_pointer_of_path p) |> String.concat ", ")
                 *               (acc.names |> List.map (fun (p, _) -> Q.json_pointer_of_path p) |> String.concat ", ")
                 *           ); *)
                let names = t'.names in
                Data.replace_seq acc.visited (Data.to_seq t'.visited);
                {names; visited=acc.visited}
              ) t
          in

          (* (\* should now be able to make the object specs *\)
           * let fields =
           *   properties
           *   |> List.map (fun (pname, _ty, required, _extra) ->
           *       let prop_path = path @ [`Field "properties"; `Field pname ] in
           *       let field_type = ModuleName.of_path ~path:prop_path in
           *       Prop_spec.make ~field_name:pname ~field_type ~required ())
           * in
           * let specs = LeafNodes.{field_name=(ModuleName.of_path ~path); fields} in
           * let leaf : LeafNodes.t =
           *   if n > 10 then
           *     `LargeObject specs
           *   else
           *     `Object specs
           * in
           * add_leaf_node t ~path ~leaf; *)
          add_sorted_module_name t' ~path ~desc:"object"
        )

      )
    | Array (_, _) -> failwith "TODO array"
    | Monomorphic_array (element, {min_items; max_items; unique_items; additional_items}) -> (
        assert (0 = min_items);
        assert (Option.is_none max_items);
        assert (not unique_items);
        assert (Option.is_none additional_items);
        Logs.debug (fun m -> m "process mono-morphic array under '%s'"  (Q.json_pointer_of_path ~wildcards:true path));
        let new_path = path @ [`Field "items"] in
        let t' = process_element t ~schema_js ~path:new_path element in
        (* TODO should now be able to make the array specs *)
        (* let field_name = ModuleName.of_path ~path in
         * let inner_type = ModuleName.of_path ~path:new_path in
         * let leaf = LeafNodes.(`Array {field_name; inner_type}) in
         * add_leaf_node t ~path ~leaf; *)
        add_sorted_module_name t' ~path ~desc:"array items"
      )
    | Combine (c, elements) -> (
        match c with
        | All_of -> (
            Logs.debug (fun m -> m "process combination allOf with %d elements under '%s'"  (List.length elements) (Q.json_pointer_of_path ~wildcards:true path));
            let (_, t') =
              elements
              |> List.fold_left (fun (i, acc) el ->
                  let new_path = path @ [`Field "allOf"; `Index i] in
                  let t' = process_element acc ~schema_js ~path:new_path el in
                  (* Logs.debug (fun m -> m "ALLOF got new names [%s], replacing old names [%s]"
                   *               (t'.names |> List.map (fun (p, _) -> Q.json_pointer_of_path p) |> String.concat ", ")
                   *               (acc.names |> List.map (fun (p, _) -> Q.json_pointer_of_path p) |> String.concat ", ")
                   *           ); *)
                  let names = t'.names in
                  Data.replace_seq acc.visited (Data.to_seq t'.visited);
                  (i+1, {names; visited=acc.visited})
                ) (0, t)
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
              add_sorted_module_name t' ~path ~desc:"allof"
          )
        | One_of -> (
            Logs.debug (fun m -> m "process combination oneOf with %d elements under '%s'"  (List.length elements) (Q.json_pointer_of_path ~wildcards:true path));
            let (_, t') =
              elements
              |> List.fold_left (fun (i, acc) el ->
                  let new_path = path @ [`Field "oneOf"; `Index i] in
                  let t' = process_element t ~schema_js ~path:new_path el in
                  let names = List.concat [t'.names; acc.names] in
                  Data.replace_seq acc.visited (Data.to_seq t'.visited);
                  (i+1, {names; visited=acc.visited})
                ) (0, t)
            in
            (* let nts =
             *   elements
             *   |> List.mapi (fun i _el ->
             *       let new_path = path @ [`Field "oneOf"; `Index i] in
             *       let field_name = ModuleName.of_path ~path in
             *       let field_type = ModuleName.of_path ~path:new_path in
             *       ({field_name; field_type} : LeafNodes.name_type_specs)
             *     )
             * in
             * let leaf = `OneOf nts in
             * add_leaf_node t ~path ~leaf; *)
            add_sorted_module_name t' ~path ~desc:"oneof"
          )
        | Any_of -> (
            Logs.debug (fun m -> m "TODO combinator ANY_OF @ %s with %d choices" (Q.json_pointer_of_path ~wildcards:true path) (List.length elements));
            (* let names = elements
             * |> List.map (fun (el:S.element) -> match el.kind with
             *     | Monomorphic_array _ -> "array"
             *     | Boolean -> "boolean"
             *     | Integer _ -> "integer"
             *     | Null -> "null"
             *     | Number _ -> "number"
             *     | Object _ -> "object"
             *     | String _ -> "string"
             *     | _ -> failwith "unknown AnyOf name"
             *   )
             * |> List.sort (String.compare)
             * in
             * (\* TODO be better *\)
             * let leaf : LeafNodes.t =
             *   match (String.concat "," names) with
             *   | "array,boolean,integer,null,number,object,string" -> `Json
             *   | _ -> `Field {encoder="string"}
             * in
             * add_leaf_node t ~path ~leaf; *)
            add_sorted_module_name t ~path ~desc:"anyof"
          )
        | Not -> failwith (Printf.sprintf "TODO combinator NOT @ %s" (Q.json_pointer_of_path ~wildcards:true path))
      )
    | Def_ref ref_path -> (
        let ref_path_str = Q.json_pointer_of_path ref_path in
        Logs.debug (fun m -> m "Dependencies - def_ref: path '%s', ref_path: '%s'" (Q.json_pointer_of_path path) ref_path_str);
        let is_cyclic = match get_visit_status t ~path:ref_path with
        | Started -> Logs.debug (fun m -> m "1) Cyclic dependency - def_ref: path '%s', ref_path: '%s'" (Q.json_pointer_of_path path) ref_path_str); true
        | _ -> false
        in

        let t' = if is_cyclic then t else process_definition t ~schema_js ~path:ref_path in
        (* let field_name = ModuleName.of_path ~path in
         * let field_type = ModuleName.of_path ~path:ref_path in
         * let leaf = LeafNodes.(`Ref ({field_name; field_type}, is_cyclic)) in
         * add_leaf_node t ~path ~leaf; *)
        add_sorted_module_name t' ~path ~desc:(Printf.sprintf "ref%s" (if is_cyclic then " - cyclic" else ""))
      )

    | Id_ref _ -> failwith "TODO Id_ref"
    | Ext_ref _ -> failwith "TODO Ext_ref"
    | String _ ->
      (* NOTE if its an _enum set then parse as `EnumSuggestions:
         the _enum fields arent picked up by Json_schema module
         and so using Json_schema.to_json wont work.
         Have to use Ezjsonm.from_channel to read the raw json and then query that.
         Also note that certain enums are scattered across the document (Command.t and Event.t)
         Also note that have to manually add some stuff to a path as
         recursion progresses (e.g. "properties" or "allOf" > index 1)
         because Json_schema doesnt hold that info
      *)
      let names =
        try
          let enum_path = path @ [`Field "_enum"] in
          match Q.query enum_path schema_js with
          | `A names ->
            let names =
              List.map Ezjsonm.decode_string_exn names
            in
            Some (String.concat ", " names)
          | _ -> None
        with _ -> None
      in
      let desc =
        names
        |> Option.map (fun nms -> (Printf.sprintf "string field with suggestions {%s}" nms))
        |> Option.value ~default:"string field"
      in
      (* add_leaf_node t ~path ~leaf; *)
      add_sorted_module_name t ~path ~desc

    | Integer _ ->
      (* add_leaf_node t ~path ~leaf:LeafNodes.(`Field {encoder="int64"}); *)
      add_sorted_module_name t ~path ~desc:"int field"
    | Number _ ->
      (* add_leaf_node t ~path ~leaf:LeafNodes.(`Field {encoder="int64"}); *)
      add_sorted_module_name t ~path ~desc:"number field"
    | Boolean ->
      (* add_leaf_node t ~path ~leaf:LeafNodes.(`Field {encoder="bool"}); *)
      add_sorted_module_name t ~path ~desc:"bool field"
    | Null -> failwith "TODO Null"
    | Any -> failwith "TODO Any"
    | Dummy -> failwith "TODO Dummy"

  let make ~schema_js =
    let names = function
      | `O fields -> fields |> List.map (fun (nm, _) -> nm)
      | _ -> []
    in
    let t = {names=[]; visited=Data.create 500} in
    (* NOTE know all /definitions/ are objects and are the only top-level things *)
    let ns = Q.query [`Field "definitions"] schema_js |> names in
    Logs.debug (fun m -> m "\n\nprocessing '%d' names" @@ List.length ns);
    ns |> List.fold_left (fun acc nm ->
        let path = [`Field "definitions"; `Field nm] in
        let t' = process_definition acc ~schema_js ~path in
        let names = t'.names in
        Data.replace_seq acc.visited (Data.to_seq t'.visited);
        {names; visited=acc.visited}
      ) t

end
