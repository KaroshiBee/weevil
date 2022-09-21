module Sp = Dap_specs
module Q = Json_query
module S = Json_schema

module D = Hashtbl.Make (struct
  type t = string

  let equal = String.equal

  let hash = Hashtbl.hash
end)

module Names = struct
  type el = {
    path : Q.path;
    desc : string;
  }

  type t = el list

  let add_sorted_module_name t ~path ~desc = {path; desc} :: t

  let get t ~desc =
    t
    |> List.filter (fun t -> t.desc = desc)
    |> List.map (fun t -> Q.json_pointer_of_path t.path)

  let begins_with t ~prefix =
    t
    |> List.filter (fun t ->
           Stringext.chop_prefix t.desc ~prefix |> Option.is_some)
    |> List.map (fun t -> Q.json_pointer_of_path t.path)
end

module Visited = struct
  type visit_status = Started | Finished | Unknown

  type t = visit_status D.t

  let check_and_add_visited t ~path =
    (* add to the visited tbl if not already there,
       returns whether a new one was added, *)
    let dfn = Q.json_pointer_of_path path in
    let is_present = D.mem t dfn in
    if not is_present then D.replace t dfn Started ;
    (t, not is_present)

  let assert_and_close_visited t ~path =
    (* asserts is in the visited tbl
       and then sets its visit_status to Finished *)
    let dfn = Q.json_pointer_of_path path in
    assert (D.mem t dfn) ;
    D.replace t dfn Finished ;
    t

  let get_visit_status t ~path =
    let dfn = Q.json_pointer_of_path path in
    D.find_opt t dfn |> Option.value ~default:Unknown
end

(* TODO add Error to Command.t enum  *)
(* TODO add Protocol_message_type enum *)

module Dfs = struct
  let _CMD = "/definitions/FAKE/command" (* fake Command enum path *)

  let _EV = "/definitions/FAKE/event" (* fake Event enum path *)

  let _MSG = "/definitions/FAKE/protocol_message_type"

  type t = {
    names : Names.t;
    visited : Visited.t;
    specs : Sp.t D.t;
    }

  let rec process_definition t ~schema_js ~path =
    (* this will be a *Request/*Response/*Event/Object top level definition
       will want to ignore base class ProtocolMessage, Request, Response, Event types
       as they are handled with the functors in Dap_t
       only want to recurse down into the defn if it is not already being/been visited
       and, if so, we want to recurse after setting the initial defn in elements map
       with the notion that some objects might get rewritten as enums
    *)
    let dfn = Q.json_pointer_of_path path in
    let dirty_name = Sp.dirty_name ~path |> Option.get in

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
        let (visited, did_add) =
          Visited.check_and_add_visited t.visited ~path
        in
        if did_add then (
          Logs.debug (fun m -> m "visiting: '%s'" dfn) ;
          let spec = Sp.make ~dirty_name ~path () in
          Logs.debug (fun m -> m "replacing spec for %s with %s" dfn (Sp.to_string spec)) ;
          D.replace t.specs dfn spec ;
          let t = process_element {t with visited} ~schema_js ~path element in
          let visited = Visited.assert_and_close_visited t.visited ~path in
          Logs.debug (fun m -> m "finished: '%s'" dfn) ;
          {t with visited})
        else (
          Logs.debug (fun m -> m "already visiting/visited: '%s'" dfn) ;
          t)
    in
    Logs.debug (fun m -> m "process dfn end: '%s'" dfn) ;
    t

  and process_element t ~schema_js ~path el =
    (* can get proper enums here, still need to qry for _enums *)
    let dfn = Q.json_pointer_of_path path in
    let dirty_name = Sp.dirty_name ~path |> Option.get in
    Logs.debug (fun m ->
        m
          "process element under '%s'"
          (Q.json_pointer_of_path ~wildcards:true path)) ;
    let enums = el.enum |> Option.value ~default:[] in
    let enums = enums
                |> List.map Json_repr.from_any
                |> List.map Ezjsonm.decode_string_exn
    in
    let n = List.length enums in
    (* it is an enum of some sort *)
    if n > 0 then (
      Logs.debug (fun m ->
          m
            "element under '%s' has enum [%s]"
            (Q.json_pointer_of_path ~wildcards:true path)
            (enums |> String.concat ", ")) ;
      let enum_spec = Sp.Enum_spec.of_path ~dirty_name ~path ~dirty_names:enums () in
      let ky =
        match
          (Sp.Enum_spec.is_command enum_spec, Sp.Enum_spec.is_event enum_spec)
        with
        | (true, false) -> _CMD
        | (false, true) -> _EV
        | (_, _) -> dfn
      in
      let to_add =
        match D.find_opt t.specs ky with
        | Some (Sp.Enum enum_spec') ->
            Sp.Enum_spec.append_enums enum_spec' ~enums:enum_spec.enums
        | Some (Sp.Object _) -> enum_spec
        | Some _ ->
            failwith
              (Printf.sprintf
                 "Expecting enum spec for ky '%s', got Req/Resp/Ev"
                 ky)
        | None -> enum_spec
      in
      D.replace t.specs ky (Sp.Enum to_add) ;
      if n > 1 then
        let names = Names.add_sorted_module_name t.names ~path ~desc:"enum" in
        {t with names}
      else t)
    else
      (* not an enum, check for special things like
         .../body on Event,
         .../body or .../message on Response
         .../arguments on Request
      *)
      let _ = if not @@ D.mem t.specs dfn then
        let vl = Sp.Object (Sp.Obj_spec.of_path ~dirty_name ~path ()) in
        D.replace t.specs dfn vl
        else
          ()
      in
      let child =
        match D.find_opt t.specs dfn with
        | Some (Sp.Object obj_specs) -> Some obj_specs
        | _ -> None
      in
      let parent_pth = Sp.root_path ~path in
      let parent_ky = Q.json_pointer_of_path parent_pth in
      Logs.debug (fun m -> m "lokking for parent '%s' of %s" parent_ky dfn);
      let parent = D.find t.specs parent_ky in
      (* should be there *)
      let new_parent =
        match (parent, child) with
        | (Sp.Request spec, Some obj_specs) ->
            let spec =
              Sp.Req_spec.(
                if is_arguments_for spec ~obj_specs then
                  set_args spec ~args:obj_specs
                else spec)
            in
            Sp.Request spec
        | (Sp.Response spec, Some obj_specs) ->
            Logs.debug (fun m -> m "response type");
            let spec =
              Sp.Resp_spec.(
                if is_body_for spec ~obj_specs then (
                  Logs.debug (fun m -> m "replacing body spec");
                  set_body spec ~body:obj_specs
                )
                else if is_message_for spec ~obj_specs then
                  set_message spec ~message:obj_specs
                else spec)
            in
            Sp.Response spec
        | (Sp.Event spec, Some obj_specs) ->
            let spec =
              Sp.Event_spec.(
                if is_body_for spec ~obj_specs then
                  set_body spec ~body:obj_specs
                else spec)
            in
            Sp.Event spec
        | (p, _) -> Logs.warn (fun m -> m "TODO Couldnt find parent %s or child %s" parent_ky dfn);  p
      in
      Logs.debug (fun m -> m "replacing parent key %s with new parent %s" parent_ky (Sp.to_string new_parent));
      D.replace t.specs parent_ky new_parent ;
      process_kind t ~schema_js ~path el.kind


  and process_kind t ~schema_js ~path = function
    | Object
        {
          properties;
          pattern_properties;
          additional_properties;
          min_properties;
          max_properties;
          schema_dependencies;
          property_dependencies;
        } ->
        assert (0 = List.length pattern_properties) ;
        assert (0 = List.length schema_dependencies) ;
        assert (0 = List.length property_dependencies) ;
        assert (0 = min_properties) ;
        assert (Option.is_none max_properties) ;
        assert (Option.is_some additional_properties) ;
        Logs.debug (fun m ->
            m
              "process object with %d properties under '%s'"
              (List.length properties)
              (Q.json_pointer_of_path ~wildcards:true path)) ;
        let n = List.length properties in
        let t =
          if n = 0 then
            let names =
              Names.add_sorted_module_name t.names ~path ~desc:"empty obj"
            in
            {t with names}
          else
            let t =
              properties
              |> List.fold_left
                   (fun acc (pname, ty, _required, _extra) ->
                     Logs.debug (fun m ->
                         m
                           "process property '%s' under '%s'"
                           pname
                           (Q.json_pointer_of_path ~wildcards:true path)) ;
                     let new_path =
                       path @ [`Field "properties"; `Field pname]
                     in
                     let t = process_element acc ~schema_js ~path:new_path ty in
                     (* Logs.debug (fun m -> m "PROPS: got new names [%s], replacing old names [%s]"
                      *               (t.names |> List.map (fun (p, _) -> Q.json_pointer_of_path p) |> String.concat ", ")
                      *               (acc.names |> List.map (fun (p, _) -> Q.json_pointer_of_path p) |> String.concat ", ")
                      *           ); *)
                     let names = t.names in
                     D.replace_seq acc.visited (D.to_seq t.visited) ;
                     {t with names; visited = acc.visited})
                   t
            in

            let names =
              Names.add_sorted_module_name t.names ~path ~desc:"object"
            in
            {t with names}
        in
        t
    | Array (_, _) -> failwith "TODO array"
    | Monomorphic_array
        (element, {min_items; max_items; unique_items; additional_items}) ->
        assert (0 = min_items) ;
        assert (Option.is_none max_items) ;
        assert (not unique_items) ;
        assert (Option.is_none additional_items) ;
        Logs.debug (fun m ->
            m
              "process mono-morphic array under '%s'"
              (Q.json_pointer_of_path ~wildcards:true path)) ;
        let new_path = path @ [`Field "items"] in
        let t = process_element t ~schema_js ~path:new_path element in
        (* TODO should now be able to make the array specs *)
        (* let field_name = ModuleName.of_path ~path in
         * let inner_type = ModuleName.of_path ~path:new_path in
         * let leaf = LeafNodes.(`Array {field_name; inner_type}) in
         * add_leaf_node t ~path ~leaf; *)
        let names =
          Names.add_sorted_module_name t.names ~path ~desc:"array items"
        in
        {t with names}
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
                   (fun (i, acc) el ->
                     let new_path = path @ [`Field "allOf"; `Index i] in
                     let t = process_element acc ~schema_js ~path:new_path el in
                     (* Logs.debug (fun m -> m "ALLOF got new names [%s], replacing old names [%s]"
                      *               (t.names |> List.map (fun (p, _) -> Q.json_pointer_of_path p) |> String.concat ", ")
                      *               (acc.names |> List.map (fun (p, _) -> Q.json_pointer_of_path p) |> String.concat ", ")
                      *           ); *)
                     let names = t.names in
                     D.replace_seq acc.visited (D.to_seq t.visited) ;
                     (i + 1, {t with names; visited = acc.visited}))
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
            let names =
              Names.add_sorted_module_name t.names ~path ~desc:"allof"
            in
            {t with names}
        | One_of ->
            Logs.debug (fun m ->
                m
                  "process combination oneOf with %d elements under '%s'"
                  (List.length elements)
                  (Q.json_pointer_of_path ~wildcards:true path)) ;
            let (_, t) =
              elements
              |> List.fold_left
                   (fun (i, acc) el ->
                     let new_path = path @ [`Field "oneOf"; `Index i] in
                     let t = process_element t ~schema_js ~path:new_path el in
                     let names = List.concat [t.names; acc.names] in
                     D.replace_seq acc.visited (D.to_seq t.visited) ;
                     (i + 1, {t with names; visited = acc.visited}))
                   (0, t)
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
            let names =
              Names.add_sorted_module_name t.names ~path ~desc:"oneof"
            in
            {t with names}
        | Any_of ->
            Logs.debug (fun m ->
                m
                  "TODO combinator ANY_OF @ %s with %d choices"
                  (Q.json_pointer_of_path ~wildcards:true path)
                  (List.length elements)) ;
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
            let names =
              Names.add_sorted_module_name t.names ~path ~desc:"anyof"
            in
            {t with names}
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
        let is_cyclic =
          match Visited.get_visit_status t.visited ~path:ref_path with
          | Started ->
              Logs.debug (fun m ->
                  m
                    "1) Cyclic dependency - def_ref: path '%s', ref_path: '%s'"
                    (Q.json_pointer_of_path path)
                    ref_path_str) ;
              true
          | _ -> false
        in

        let t =
          if is_cyclic then t
          else process_definition t ~schema_js ~path:ref_path
        in
        (* let field_name = ModuleName.of_path ~path in
         * let field_type = ModuleName.of_path ~path:ref_path in
         * let leaf = LeafNodes.(`Ref ({field_name; field_type}, is_cyclic)) in
         * add_leaf_node t ~path ~leaf; *)
        let names =
          Names.add_sorted_module_name
            t.names
            ~path
            ~desc:
              (Printf.sprintf "ref%s" (if is_cyclic then " - cyclic" else ""))
        in
        {t with names}
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
                let names = List.map Ezjsonm.decode_string_exn names in
                Some (String.concat ", " names)
            | _ -> None
          with _ -> None
        in
        let desc =
          names
          |> Option.map (fun nms ->
                 Printf.sprintf "string field with suggestions {%s}" nms)
          |> Option.value ~default:"string field"
        in
        (* add_leaf_node t ~path ~leaf; *)
        let names = Names.add_sorted_module_name t.names ~path ~desc in
        {t with names}
    | Integer _ ->
        (* add_leaf_node t ~path ~leaf:LeafNodes.(`Field {encoder="int64"}); *)
        let names =
          Names.add_sorted_module_name t.names ~path ~desc:"int field"
        in
        {t with names}
    | Number _ ->
        (* add_leaf_node t ~path ~leaf:LeafNodes.(`Field {encoder="int64"}); *)
        let names =
          Names.add_sorted_module_name t.names ~path ~desc:"number field"
        in
        {t with names}
    | Boolean ->
        (* add_leaf_node t ~path ~leaf:LeafNodes.(`Field {encoder="bool"}); *)
        let names =
          Names.add_sorted_module_name t.names ~path ~desc:"bool field"
        in
        {t with names}
    | Null -> failwith "TODO Null"
    | Any -> failwith "TODO Any"
    | Dummy -> failwith "TODO Dummy"

  let make ~schema_js =
    let names = function
      | `O fields -> fields |> List.map (fun (nm, _) -> nm)
      | _ -> []
    in
    let t = {names = []; visited = D.create 500; specs = D.create 500} in
    (* NOTE know all /definitions/ are objects and are the only top-level things *)
    let ns = Q.query [`Field "definitions"] schema_js |> names in
    Logs.info (fun m -> m "\n\nprocessing '%d' names" @@ List.length ns) ;
    ns
    |> List.fold_left
         (fun acc nm ->
           let path = [`Field "definitions"; `Field nm] in
           let t = process_definition acc ~schema_js ~path in
           let names = t.names in
           D.replace_seq acc.visited (D.to_seq t.visited) ;
           {t with names; visited = acc.visited})
         t
end

