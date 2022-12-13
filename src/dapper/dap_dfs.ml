(* TODO
   sort out mess of unweird names, make module name, make dirty etc
*)

module Sp = Dap_specs
module Q = Json_query
module S = Json_schema

module CommandHelper = struct

  let module_name = "Dap_commands"

  let struct_decl_str name ~on =
    match Stringext.cut name ~on with
    | Some (enum_str, "") ->
      let enum_str = Sp.unweird_name enum_str in
      Printf.sprintf "struct type t = %s.t let value=%s.%s let enc = %s.enc end" module_name module_name enum_str module_name
    | _ -> assert false

  let enum_str name ~on =
    match Stringext.cut name ~on with
    | Some (enum_str, "") ->
      let enum_str = Sp.unweird_name enum_str in
      String.uncapitalize_ascii enum_str
    | _ -> assert false

end

module EventHelper = struct

  let module_name = "Dap_events"

  let struct_decl_str name =
    match Stringext.cut name ~on:"Event" with
    | Some (enum_str, "") ->
      let enum_str = Sp.unweird_name enum_str in
      Printf.sprintf "struct type t = %s.t let value=%s.%s let enc = %s.enc end" module_name module_name enum_str module_name
    | _ -> assert false

  let enum_str name =
    match Stringext.cut name ~on:"Event" with
    | Some (enum_str, "") ->
      let enum_str = Sp.unweird_name enum_str in
      String.uncapitalize_ascii enum_str
    | _ -> assert false
end

module Visited = struct

  type status = | Started | Finished | Unknown [@@deriving show]

  type t = (string, status) Hashtbl.t

  let pp ppf values =
    Hashtbl.iter (fun ky vl ->
        Format.fprintf ppf "@[<1>%s: %s@]@." ky (show_status vl)
          )
      values
end


module Finished = struct

  type t = (string, Sp.t) Hashtbl.t

  let pp ppf values =
    Hashtbl.iter (fun ky vl ->
        Format.fprintf ppf "@[<1>%s: %s@]@." ky (Sp.show vl)
          )
      values
end


module RestartArgumentsHelper = struct
  let is_restart_arguments = function
    | [`Field "definitions"; `Field "RestartArguments"] -> true
    | _ -> false

  let is_launch_arguments = function
    | [`Field "definitions"; `Field "LaunchRequestArguments"] -> true
    | _ -> false

  let is_attach_arguments = function
    | [`Field "definitions"; `Field "AttachRequestArguments"] -> true
    | _ -> false

  let is_special_definition path =
    is_attach_arguments path
    || is_launch_arguments path
    || is_restart_arguments path

end


module Dfs = struct

  type t = {
    schema_js: Json_repr.ezjsonm; [@printer Json_repr.pp (module Json_repr.Ezjsonm)]
    command_enum: Sp.Enum_spec.t;
    event_enum: Sp.Enum_spec.t;
    nodes: Sp.t list;
    ordering: string list;
    visited: Visited.t;
    finished: Finished.t;
  } [@@deriving show]

  let ordering t = CommandHelper.module_name :: EventHelper.module_name :: (t.ordering |> List.rev)

  let _set_field_type t module_name = function
    | (type_, enc_) ->
      match t.nodes with
      | Sp.Field field_spec :: nodes ->
        let field = Sp.Field_spec.{field_spec with module_name; type_; enc_} in
        let nodes = (Sp.Field field) :: nodes in
        {t with nodes}
      | _ -> t

  let _set_seq t =
    match t.nodes with
    | Sp.Field field_spec :: nodes ->
      let field = Sp.Field_spec.{field_spec with seq = true} in
      let nodes = Sp.Field field :: nodes in
      {t with nodes}
    | _ -> t

  let _set_cyclic t =
    match t.nodes with
    | Sp.Field field_spec :: nodes ->
      let field = Sp.Field_spec.{field_spec with cyclic = true} in
      let nodes = Sp.Field field :: nodes in
      {t with nodes}
    | _ -> t

  let _set_nullable_field_type t module_name = function
    | (type_, enc_) ->
      (* NOTE make it into a required optional *)
      let type_ = type_^" option" in
      let enc_ = "(option "^enc_^")" in
      match t.nodes with
      | Sp.Field field_spec :: nodes ->
        let field = Sp.Field_spec.{field_spec with module_name; type_; enc_; required=true} in
        let nodes = (Sp.Field field) :: nodes in
        {t with nodes}
      | _ -> t

  let _set_variant_field_type t module_name = function
    | [("int", "int31"); ("string", "string")] ->
        let nm = Dap_base.IntString.module_name in
        _set_field_type t module_name (nm^".t", nm^".enc")
    | _ ->
      failwith "TODO _set_variant_field_type"

  let _set_nullable_field_dict_type t _module_name _type_encs =
    Logs.debug (fun m -> m "TODO not sure how to encode an object-as-dict");
    _set_field_type t "" ("Data_encoding.json", "json")

  let _set_field_dict_type t _module_name _type_encs =
    Logs.debug (fun m -> m "TODO not sure how to encode an object-as-dict");
    _set_field_type t "" ("Data_encoding.json", "json")

  let rec process_definition t ~path =
    (* this will be a *Request/*Response/*Event/Object top level definition
       will want to ignore base class ProtocolMessage, Request, Response, Event types
       as they are handled with the functors in Dap_t
       only want to recurse down into the defn if it is not already being/been visited
       and, if so, we want to recurse after setting the initial defn in elements map
       with the notion that some objects might get rewritten as enums
    *)
    let dfn = Q.json_pointer_of_path path in
    let module_name = Sp.make_module_name path in

    Logs.debug (fun m -> m "process dfn start: '%s', '%s'" dfn module_name) ;
    let t =
      if Sp.is_special_definition ~path then (
        Logs.debug (fun m -> m "special case: '%s'" dfn) ;
        t)
      else if RestartArgumentsHelper.is_special_definition path then (
        Logs.debug (fun m -> m "special case restart: '%s'" dfn) ;
        t)
      else
        (* check is valid name by finding the definition *)
        let schema = Json_schema.of_json t.schema_js in
        let element = S.find_definition dfn schema in
        (* if added new one then recurse too *)
        let has_visited = Hashtbl.mem t.visited dfn in
        if (not has_visited) then (
          Logs.debug (fun m -> m "visiting: '%s'" dfn) ;
          Hashtbl.add t.visited dfn Started;
          let t = process_element t ~path element in
          Hashtbl.replace t.visited dfn Finished;
          Logs.debug (fun m -> m "finished: '%s'" dfn) ;
          match t.nodes with
          | (Sp.Object _ as spec) :: nodes
          | (Sp.Request _ as spec) :: nodes
          | (Sp.Response _ as spec) :: nodes
          | (Sp.Event _ as spec) :: nodes ->
            Logs.debug (fun m -> m "adding definition '%s' to finished pile with module name '%s'" dfn module_name) ;
            let ordering = module_name :: t.ordering in
            Hashtbl.add t.finished module_name spec;
            {t with nodes; ordering}
          | _ -> t
        )
        else (
          Logs.debug (fun m -> m "already visiting/visited: '%s'" dfn) ;
          t
        )
    in
    Logs.debug (fun m -> m "process dfn end: '%s'" dfn) ;
    t

  and process_enums t ~path =
    let dfn = Q.json_pointer_of_path path in
    let module_name = Sp.make_module_name path in
    let _aux dirty_names =
      let enum_spec = Sp.Enum_spec.of_path ~dirty_name:module_name ~path ~dirty_names () in
      let ordering = module_name :: t.ordering in
      Hashtbl.add t.finished module_name (Sp.Enum enum_spec);
      {t with ordering}
    in
    function
    | [dirty_name] as dirty_names -> (
      Logs.debug (fun m -> m "element under '%s' has enum [%s]" dfn dirty_name);
      let enum_spec = Sp.Enum_spec.of_path ~dirty_name:module_name ~path ~dirty_names () in
      match
        (Sp.Enum_spec.is_command enum_spec, Sp.Enum_spec.is_event enum_spec)
      with
      | (true, false) ->
        Logs.debug (fun m -> m "element under '%s' has command enum [%s]" dfn dirty_name);
        let command_enum = t.command_enum |> Sp.Enum_spec.append_enum ~dirty_name in
        {t with command_enum}
      | (false, true) ->
        Logs.debug (fun m -> m "element under '%s' has event enum [%s]" dfn dirty_name);
        let event_enum = t.event_enum |> Sp.Enum_spec.append_enum ~dirty_name in
        {t with event_enum}
      | (_, _) ->
        Logs.debug (fun m -> m "element under '%s' has single enum field [%s]" dfn dirty_name);
        _aux dirty_names
    )
    | (_ :: _rest) as dirty_names ->
      Logs.debug (fun m -> m "element under '%s' has enum [%s]" dfn (dirty_names |> String.concat ", ")) ;
      _aux dirty_names
    | _ -> t

  and process_element t ~path el =
    (* can get proper enums here, still need to qry for _enums c.f. string handler below *)
    let dfn = Q.json_pointer_of_path path in
    Logs.debug (fun m -> m "process element under '%s'" dfn);
    let enums = el.enum
      |> Option.value ~default:[]
      |> List.map Json_repr.from_any
      |> List.map Ezjsonm.decode_string_exn
    in
    let n = List.length enums in
    (* it is an enum of some sort *)
    if n > 0 then (
      Logs.debug (fun m -> m "element enum element under '%s'" dfn);
      process_enums t ~path enums
    ) else (
      Logs.debug (fun m -> m "process element kind under '%s'" dfn) ;
      process_kind t ~path el.kind
    )

  and process_properties t ~path properties =
    let n = List.length properties in
    let dfn = Q.json_pointer_of_path path in
    Logs.debug (fun m -> m "process object with %d properties under '%s'" n dfn);
    properties
    |> List.fold_left
      (fun t_acc (pname, el, required, _extra) ->
         Logs.debug (fun m -> m "process property '%s' under '%s'" pname dfn);
         let new_path =
           path @ [`Field "properties"; `Field pname]
         in
         let module_name = Sp.make_module_name new_path in
         let new_spec = Sp.Field (Sp.Field_spec.make
                                    ~path:new_path
                                    ~dirty_name:pname
                                    ~required
                                    ~module_name
                                    ~type_:(module_name^".t")
                                    ~enc_:(module_name^".enc")
                                    ()
                                 ) in
         let new_nodes =  new_spec :: t_acc.nodes in
         let t' = process_element {t_acc with nodes=new_nodes} ~path:new_path el in
         Hashtbl.replace_seq t_acc.visited (Hashtbl.to_seq t'.visited) ;
         {t' with visited = t_acc.visited}
      ) t

  and process_empty_object t ~path =
    let dfn = Q.json_pointer_of_path path in
    let module_name = Sp.make_module_name path in
    Logs.debug (fun m -> m "process empty object with no additionalProperties under '%s' and module name '%s'" dfn module_name);
    let o = Sp.Object (Sp.Obj_spec.of_path ~dirty_name:module_name ~path ()) in
    let nodes = o :: t.nodes in
    {t with nodes}

  and process_kind t ~path = function
    | Object o when (List.length o.properties) = 0 -> (
        let dfn = Q.json_pointer_of_path path in
        Logs.debug (fun m -> m "process object with 0 properties under '%s'" dfn);
        (* NOTE things like Message.variables have additionalProperties,
           they are used for specifying dicts of strings,
           sometimes dicts of nullable strings *)
        match o.additional_properties with
        | Some _el -> (
            Logs.debug (fun m -> m "process object.additionalProperties under '%s'" dfn);
            let type_path = path @ [`Field "additionalProperties"; `Field "type"] in
            try
              match Q.query type_path t.schema_js with
              | `A [`String "string";
                    `String "null"] ->
                _set_nullable_field_dict_type t "" ("string", "string")
              | `String "string" ->
                _set_field_dict_type t "" ("string", "string")
              | _ -> failwith "TODO more general additionalProperties"
            with Not_found ->
              process_empty_object t ~path
          )
        | None ->
            process_empty_object t ~path
      )
    | Object
        {
          properties;
          pattern_properties;
          additional_properties;
          min_properties;
          max_properties;
          schema_dependencies;
          property_dependencies;
        } when (0 = List.length pattern_properties &&
                0 = List.length schema_dependencies &&
                0 = List.length property_dependencies &&
                0 = min_properties &&
                Option.is_none max_properties &&
                Option.is_some additional_properties) -> (
        let n = List.length properties in
        let m = List.length t.nodes in
        match Sp.dirty_name ~path with
        | Some dirty_name -> (
            let dfn = Q.json_pointer_of_path path in
            let spec = Sp.make ~dirty_name ~path () in
            Logs.debug (fun m -> m "got spec for %s with %s" dfn (Sp.show spec)) ;
            let nodes = spec :: t.nodes in
            let t = process_properties {t with nodes} ~path properties in
            assert (List.length t.nodes >= m+n) ;
            (* TODO need to filter out all initial objects, these are objects that have been inline-defined,
               all other finished items have already been put on the finished pile because they can be
               identified as non-objects but inline defined objects end up in this case branch so
               need to be dealt with here *)
            let rec aux t =
              match t.nodes with
              | Sp.Object obj :: nodes ->
                let module_name = Sp.make_module_name obj.path in
                Logs.debug (fun m -> m "adding inline object at '%s' to finished pile with module name '%s'" dfn module_name) ;
                let ordering = module_name :: t.ordering in
                Hashtbl.add t.finished module_name (Sp.Object obj);
                aux {t with nodes; ordering}
              | _ -> t
            in
            let t = aux t in
            (* now pop off the n-top Fields of the node list and add to the object spec *)
            let arr = Array.of_list t.nodes in
            Logs.debug (fun m -> m "n:%d, t:%s" n @@ show t);
            let fields = Array.sub arr 0 n |> Array.to_list |> List.map (fun f ->
                match f with
                | Sp.Field spec -> spec
                | s ->  failwith (Printf.sprintf "reading %d items, expected field, got %s" n (Sp.show s))
              ) |> List.rev
            in
            match Array.sub arr n (Array.length arr - n) |> Array.to_list with
            | Sp.Object spec :: nodes' ->
              Logs.debug (fun m -> m "adding %d fields to object at '%s'" n @@ Q.json_pointer_of_path path) ;
              let is_cyclic = List.exists (fun (f:Sp.Field_spec.t) -> f.cyclic) fields in
              let nodes = Sp.(Object Obj_spec.{spec with fields; is_cyclic} ) :: nodes' in
              {t with nodes; }
            | _ -> Logs.warn (fun m -> m "Couldnt find final object spec from path '%s'" @@ Q.json_pointer_of_path path); t
          )
        | None -> Logs.warn (fun m -> m "Couldnt make dirty name from path '%s'" @@ Q.json_pointer_of_path path); t
      )
    | Object _ -> failwith "TODO object"
    | Array (_, _) -> failwith "TODO array"
    | Monomorphic_array (element, {min_items; max_items; unique_items; additional_items})
        when (0 = min_items &&
              Option.is_none max_items &&
              not unique_items &&
              Option.is_none additional_items) ->
        let dfn = Q.json_pointer_of_path path in
        Logs.debug (fun m -> m "process mono-morphic array under '%s'" dfn) ;
        let new_path = path @ [`Field "items"] in
        let t = process_element t ~path:new_path element in
        _set_seq t
    | Monomorphic_array _ -> failwith "TODO marray"

    (* NOTE the only All_of uses are for Request/Response/Event s *)
    | Combine (All_of, [dref; el])
        when dref.kind = Def_ref [`Field "definitions"; `Field "Request"] -> (
        Logs.debug (fun m -> m "process request under '%s'" (Q.json_pointer_of_path ~wildcards:true path)) ;
        let new_path = path @ [`Field "allOf"; `Index 1] in
        let t' = process_element t ~path:new_path el in
        Hashtbl.replace_seq t.visited (Hashtbl.to_seq t'.visited) ;
        match t'.nodes with
        | Sp.Object specs :: rest ->
          let nodes = Sp.Request specs :: rest in
          {t' with nodes}
        | _ -> t'
      )
    | Combine (All_of, [dref; el])
        when dref.kind = Def_ref [`Field "definitions"; `Field "Event"] -> (
        Logs.debug (fun m -> m "process event under '%s'" (Q.json_pointer_of_path ~wildcards:true path)) ;
        Logs.debug (fun m -> m "all_of event BEFORE %s" @@ show t);
        let new_path = path @ [`Field "allOf"; `Index 1] in
        let t' = process_element t ~path:new_path el in
        Hashtbl.replace_seq t.visited (Hashtbl.to_seq t'.visited) ;
        Logs.debug (fun m -> m "all_of event AFTER %s" @@ show t');
        match t'.nodes with
        | Sp.Object specs :: rest ->
          let nodes = Sp.Event specs :: rest in
          {t' with nodes}
        | _ -> t'
      )
    | Combine (All_of, [dref; el])
        when dref.kind = Def_ref [`Field "definitions"; `Field "Response"] -> (
        Logs.debug (fun m -> m "process response under '%s'" (Q.json_pointer_of_path ~wildcards:true path)) ;
        let new_path = path @ [`Field "allOf"; `Index 1] in
        let t' = process_element t ~path:new_path el in
        Hashtbl.replace_seq t.visited (Hashtbl.to_seq t'.visited) ;
        match t'.nodes with
        | Sp.Object specs :: rest ->
          let nodes = Sp.Response specs :: rest in
          {t' with nodes}
        | _ -> t'
      )
    | Combine (All_of, [dref; el])
        when dref.kind = Def_ref [`Field "definitions"; `Field "ValueFormat"] -> (
        let dfn = Q.json_pointer_of_path path in
        Logs.debug (fun m -> m "process allOf for 'ValueFormat' under '%s'" dfn);
        let new_path = path @ [`Field "allOf"; `Index 0] in
        let t' = process_element t ~path:new_path dref in
        Hashtbl.replace_seq t.visited (Hashtbl.to_seq t'.visited) ;
        let new_path = path @ [`Field "allOf"; `Index 1] in
        let t'' = process_element t' ~path:new_path el in
        Hashtbl.replace_seq t'.visited (Hashtbl.to_seq t''.visited) ;
        match t''.nodes with
        | Sp.Object specs :: rest ->
          let vspecs = match Hashtbl.find t''.finished "ValueFormat" with | Sp.Object vspecs -> vspecs | _ -> assert false in
          let nodes = Sp.Object (Sp.Obj_spec.append_fields_front specs vspecs) :: rest in
          {t'' with nodes}
        | _ -> t''
      )
    | Combine (Any_of, elements) -> (
        (* NOTE there arent any explicit Any_of declared,
           all uses are implicit with 'type': [thing, thing, thing]
           also the seven element form (array, boolean, int, null, ...)
           pops up a lot and is taken to mean any json value.
           Otherwise only have instances of [string, null] and [integer, string],
           can add generalities later if nec *)
        let path_field = "type" in
        Logs.debug (fun m -> m "process combination '%s' with %d elements under '%s'" path_field (List.length elements) (Q.json_pointer_of_path ~wildcards:true path)) ;
        let type_path = path @ [`Field path_field] in
        (* type path should be present *)
        match Q.query type_path t.schema_js with
        | `A
            [`String "array";
             `String "boolean";
             `String "integer";
             `String "null";
             `String "number";
             `String "object";
             `String "string"] ->
          _set_field_type t "" ("Data_encoding.json", "json")
        | `A [`String "string";
              `String "null"] ->
          _set_nullable_field_type t "" ("string", "string")
        | `A [`String "integer";
              `String "string"] ->
          _set_variant_field_type t "" [("int", "int31"); ("string", "string")]
        | _ -> failwith "TODO more general Any_of"
      )

    | Combine (One_of, _elements) -> failwith "TODO One_of"

    | Combine _ -> failwith "TODO other combinators"

    | Def_ref ref_path ->
        let ref_path_str = Q.json_pointer_of_path ref_path in
        Logs.debug (fun m ->
            m
              "Dependencies - def_ref: path '%s', ref_path: '%s'"
              (Q.json_pointer_of_path path)
              ref_path_str) ;
        let is_cyclic =
          match Hashtbl.find_opt t.visited ref_path_str with
          | Some Started -> true
          | _ -> false
        in

        (* Logs.debug (fun m -> m "nodes before def_ref '%s': %s" ref_path_str @@ show t); *)
        let t =
          if is_cyclic then (
            _set_cyclic t
          )
          else (
            let t' = process_definition t ~path:ref_path in
            (* get typename from ref_path *)
            let ref_type_name = Sp.make_module_name ref_path in
            _set_field_type t' ref_type_name (ref_type_name^".t", ref_type_name^".enc")
          )
        in
        (* Logs.debug (fun m -> m "nodes after def_ref '%s': %s" ref_path_str @@ show t); *)
        t

    | Id_ref _ -> failwith "TODO Id_ref"
    | Ext_ref _ -> failwith "TODO Ext_ref"
    | String _ -> (
      (* NOTE if its an _enum set then parse as Enum(suggested=true) because
         the _enum fields arent picked up by Json_schema module
         and so using Json_schema.to_json wont work.
         Have to use Ezjsonm.decode_string to read the raw json and then use that.
      *)
      let enum_path = path @ [`Field "_enum"] in
      let module_name = Sp.make_module_name enum_path in
      let enum =
        try
          match Q.query enum_path t.schema_js with
          | `A names ->
            let dirty_names =
              List.map Ezjsonm.decode_string_exn names
            in
            Some (Sp.Enum_spec.of_path ~dirty_name:module_name ~path:enum_path ~dirty_names ~suggested:true())
          | _ -> None
        with Not_found -> None
      in
      match enum with
      | Some enum ->
        Logs.debug (fun m -> m "inlined Enum");
        Hashtbl.add t.finished module_name (Sp.Enum enum);
        let ordering = module_name :: t.ordering in
        _set_field_type {t with ordering} module_name (module_name^".t", module_name^".enc")
      | None ->
        Logs.debug (fun m -> m "String");
        _set_field_type t "" ("string", "string")
    )
    | Integer _ ->
      Logs.debug (fun m -> m "Int");
      _set_field_type t "" ("int", "int31")
    | Number _ ->
      Logs.debug (fun m -> m "Number");
      _set_field_type t "" ("int", "int31")
    | Boolean ->
      Logs.debug (fun m -> m "Bool");
      _set_field_type t "" ("bool", "bool")
    | Null -> failwith "TODO Null"
    | Any -> failwith @@ Printf.sprintf "TODO Any '%s'" @@ Q.json_pointer_of_path path
    | Dummy -> failwith "TODO Dummy"

  let make ~schema_js =
    let names = function
      | `O fields -> fields |> List.map (fun (nm, _) -> nm)
      | _ -> []
    in
    let empty = {
      schema_js;
      command_enum =
        Sp.Enum_spec.of_path
          ~dirty_name:CommandHelper.module_name
          ~path:[]
          ~dirty_names:["error"]
          ();
      event_enum =
        Sp.Enum_spec.of_path
          ~dirty_name:EventHelper.module_name
          ~path:[]
          ~dirty_names:[]
          ();
      nodes = [];
      ordering = [];
      visited = Hashtbl.create 500;
      finished = Hashtbl.create 500
    }
    in
    (* NOTE know all /definitions/ are objects and are the only top-level things *)
    let ns = Q.query [`Field "definitions"] schema_js |> names in
    Logs.debug (fun m -> m "\n\nprocessing '%d' names" @@ List.length ns) ;
    ns
    |> List.fold_left
         (fun t_acc nm ->
           let path = [`Field "definitions"; `Field nm] in
           let t = process_definition t_acc ~path in
           Hashtbl.replace_seq t_acc.visited (Hashtbl.to_seq t.visited) ;
           {t with visited = t_acc.visited})
         empty

end
