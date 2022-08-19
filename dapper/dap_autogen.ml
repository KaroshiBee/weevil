module Q = Json_query

let _COMMAND = "Command"
let _EVENT = "Event"
let _EMPTY_OBJECT = "EmptyObject"


module ModuleName = struct
  type t = {
    safe_name: string;
    path: Q.path;
  }

  let _make_module_name ~path =
    let name =
      path
      |> List.filter_map (fun el ->
          match el with
          | `Star | `Index _ | `Next -> None
          | `Field f -> (
              match f with
              | "definitions" | "allOf" | "oneOf" | "items" | "_enum" | "properties" -> None
              | _ -> Some f
            )
          | _ -> None
        )
      |> String.concat "_"
    in
    String.capitalize_ascii name

  let of_path ~path =
    let safe_name =
      match List.rev path with
      | `Field "command" :: _ -> _COMMAND (* Request/Response command types *)
      | `Field "event" :: _ -> _EVENT (* Event types *)
      (* | `Field "message" :: _ ->
               *   if field = "_enum" && List.length names = 1 && List.hd names = "cancelled" then
               *     "Message_enum" (\* Response message types - only one currently *\)
               *   else
               *     make_module_name ~path:enum_path *)
      | _ -> _make_module_name ~path
    in
    {safe_name; path}

  let of_string ~js_ptr =
    of_path ~path:(Q.path_of_json_pointer js_ptr)

  let to_type_t t = Printf.sprintf "%s.t" t.safe_name
  let to_enc t = Printf.sprintf "%s.enc" t.safe_name
  let to_module_name t = Printf.sprintf "%s" t.safe_name
  let to_functor_arg = function
    | `Command command_value -> Printf.sprintf "struct let command=%s.%s end" _COMMAND command_value
    | `Event event_value -> Printf.sprintf "struct let event=%s.%s end" _EVENT event_value

end


module Field_spec = struct
  type field_name = string
  type t = {
    safe_name: field_name; (* safe to use as an ocaml record name *)
    field_name: field_name; (* original field name as per spec *)
    required: bool;
    is_cyclic: bool;
  }

  let _unweird_name nm =
    match (String.lowercase_ascii nm) with
    | "type" | "module" | "lazy" -> Printf.sprintf "%s_" nm
    | "null" -> "empty_"
    | _ -> nm

  let make ~field_name ?(required=true) ?(is_cyclic=false) () =
    let safe_name = _unweird_name field_name in
    {safe_name; field_name; required; is_cyclic}

  let make_cyclic ~field_name ?(required=true) () =
    make ~field_name ~required ~is_cyclic:true ()
end


module LeafNodes = struct

  type module_name = ModuleName.t
  type specs = {
    module_name: module_name
  }
  type object_specs = {
    module_name: module_name;
    fields: Field_spec.t list;
  }
  type array_specs = {
    field_name: Field_spec.t;
    inner_type: module_name;
  }

  type 'a maybe_spec = [ `Required of 'a | `Optional of 'a | `None]

  module RequestSpec = struct

    type t = {
      module_name: module_name;
      command_value: string;
      args_name: module_name maybe_spec;
    }

    let make ~module_js_ptr ~command_value ?args_js_ptr () =
      let module_name = ModuleName.of_string ~js_ptr:module_js_ptr in
      let command_value = String.capitalize_ascii command_value in
      let args_name = args_js_ptr |> Option.map (function
        | `Required js_ptr -> `Required (ModuleName.of_string ~js_ptr)
        | `Optional js_ptr -> `Optional (ModuleName.of_string ~js_ptr)
        | `None -> `None
        ) |> Option.value ~default:`None
      in
      {module_name; command_value; args_name}

    let render t =
      match t.args_name with
      | `Required args_name ->
        Printf.sprintf
          "module %s = MakeRequest (%s) (%s)"
          (ModuleName.to_module_name t.module_name)
          (ModuleName.to_functor_arg (`Command t.command_value))
          (ModuleName.to_module_name args_name)
      | `Optional args_name ->
        Printf.sprintf
          "module %s = MakeRequest_optionalArgs (%s) (%s)"
          (ModuleName.to_module_name t.module_name)
          (ModuleName.to_functor_arg (`Command t.command_value))
          (ModuleName.to_module_name args_name)
      | `None ->
        Printf.sprintf
          "module %s = MakeRequest_optionalArgs (%s) (%s)"
          (ModuleName.to_module_name t.module_name)
          (ModuleName.to_functor_arg (`Command t.command_value))
          _EMPTY_OBJECT
  end

  module ResponseSpec = struct
    type t = {
      module_name: module_name;
      command_value: string;
      body_name: module_name maybe_spec;
    }

    (* NOTE have to extract response command value from module name *)
    let make ~module_js_ptr ?body_js_ptr () =
      let module_name = ModuleName.of_string ~js_ptr:module_js_ptr in
      let command_value =
        let mname = ModuleName.to_module_name module_name in
        match Stringext.cut mname ~on:"Response" with
        | Some (value, _) -> value
        | None -> failwith (Printf.sprintf "Incorrect Response name %s" mname)
      in
      let body_name = body_js_ptr |> Option.map (function
        | `Required js_ptr -> `Required (ModuleName.of_string ~js_ptr)
        | `Optional js_ptr -> `Optional (ModuleName.of_string ~js_ptr)
        | `None -> `None
        ) |> Option.value ~default:`None
      in
      {module_name; command_value; body_name}


    let render t =
      match t.body_name with
      | `Required body_name ->
        Printf.sprintf
          "module %s = MakeResponse (%s) (%s)"
          (ModuleName.to_module_name t.module_name)
          (ModuleName.to_functor_arg (`Command t.command_value))
          (ModuleName.to_module_name body_name)
      | `Optional body_name ->
        Printf.sprintf
          "module %s = MakeResponse_optionalBody (%s) (%s)"
          (ModuleName.to_module_name t.module_name)
          (ModuleName.to_functor_arg (`Command t.command_value))
          (ModuleName.to_module_name body_name)
      | `None ->
        Printf.sprintf
          "module %s = MakeResponse_optionalBody (%s) (%s)"
          (ModuleName.to_module_name t.module_name)
          (ModuleName.to_functor_arg (`Command t.command_value))
          _EMPTY_OBJECT

  end

  module EventSpec = struct
    type t = {
      module_name: module_name;
      event_value: string;
      body_name: module_name maybe_spec;
    }

    let make ~module_js_ptr ~event_value ?body_js_ptr () =
      let module_name = ModuleName.of_string ~js_ptr:module_js_ptr in
      let event_value = String.capitalize_ascii event_value in
      let body_name = body_js_ptr |> Option.map (function
        | `Required js_ptr -> `Required (ModuleName.of_string ~js_ptr)
        | `Optional js_ptr -> `Optional (ModuleName.of_string ~js_ptr)
        | `None -> `None
        ) |> Option.value ~default:`None
      in
      {module_name; event_value; body_name}

    let render t =
      match t.body_name with
      | `Required body_name ->
        Printf.sprintf
          "module %s = MakeEvent (%s) (%s)"
          (ModuleName.to_module_name t.module_name)
          (ModuleName.to_functor_arg (`Event t.event_value))
          (ModuleName.to_module_name body_name)
      | `Optional body_name ->
        Printf.sprintf
          "module %s = MakeEvent_optionalBody (%s) (%s)"
          (ModuleName.to_module_name t.module_name)
          (ModuleName.to_functor_arg (`Event t.event_value))
          (ModuleName.to_module_name body_name)
      | `None ->
        Printf.sprintf
          "module %s = MakeEvent_optionalBody (%s) (%s)"
          (ModuleName.to_module_name t.module_name)
          (ModuleName.to_functor_arg (`Event t.event_value))
          _EMPTY_OBJECT

  end


  type t = [
    | `Json
    | `Enum of specs (* already dealt with *)
    | `EmptyObject of specs
    | `Object of object_specs
    (* | `CyclicObject of object_specs
     * | `LargeObject of object_specs *)
    | `Array of array_specs
    | `Request of RequestSpec.t
    | `Response of ResponseSpec.t
    | `Event of EventSpec.t
  ]

end

open Json_schema

module Dfs = struct

  module Data = Hashtbl.Make(struct type t = string let equal = String.equal let hash = Hashtbl.hash end)

  type prop_ty = EmptyObj of object_specs | Obj of object_specs

  type el =
    | SortedModuleNames of ModuleName.t list
    | Visited of ModuleName.t

  type t = el Data.t

  let _KEY = "sorted_module_names"

  let add_path t ~path =
    let dfn = Q.json_pointer_of_path ~wildcards:true path in
    let mname = ModuleName.of_path ~path in
    let module_names = match (Data.find_opt t _KEY |> Option.value ~default:(SortedModuleNames [])) with
      | SortedModuleNames names -> mname :: names
      | Visited _ -> []
    in
    Data.replace t _KEY (SortedModuleNames module_names);
    Data.replace t dfn (Visited mname);
    t

  let rec process_dfn t ~schema_js ~path =
    let dfn = Q.json_pointer_of_path ~wildcards:true path in
    Logs.info (fun m -> m "process dfn start: '%s'\n"  dfn);
    (* check is valid name *)
    let schema = Json_schema.of_json schema_js in
    let element = find_definition dfn schema in
    (* if already visited then dont recurse *)
    if not @@ Data.mem t dfn then (
      let t' = add_path t ~path in
      process_element t' ~schema_js ~path element;
    ) else (
      Logs.info (fun m -> m "already visited: '%s'\n"  dfn);
    );
    Logs.info (fun m -> m "process dfn end: '%s'\n"  dfn);


  and process_element t ~schema_js ~path el =
    Logs.debug (fun m -> m "process element under '%s'\n"  (Q.json_pointer_of_path ~wildcards:true path));
    process_kind t ~schema_js ~path el.kind

  and process_kind t ~schema_js ~path = function
    | Object {properties; pattern_properties; additional_properties; min_properties; max_properties; schema_dependencies; property_dependencies} -> (
        assert (0 = List.length pattern_properties);
        assert (0 = List.length schema_dependencies);
        assert (0 = List.length property_dependencies);
        assert (0 = min_properties);
        assert (Option.is_none max_properties);
        assert (Option.is_some additional_properties);
        Logs.debug (fun m -> m "process object with %d properties under '%s'\n"  (List.length properties) (Q.json_pointer_of_path ~wildcards:true path));
        if List.length properties = 0 then
          let obj_specs = EmptyObj {properties=[]; pattern_properties; additional_properties; min_properties; max_properties; schema_dependencies; property_dependencies} in
          process_property t ~schema_js ~path obj_specs
        else
          properties
          |> List.iter (fun (pname, ty, required, extra) ->
              let obj_specs = Obj {properties=[(pname, ty, required, extra)]; pattern_properties; additional_properties; min_properties; max_properties; schema_dependencies; property_dependencies} in
              process_property t ~schema_js ~path obj_specs
            )

      )
    | Array (_, _) -> let _ = failwith "TODO array" in ()
    | Monomorphic_array (element, {min_items; max_items; unique_items; additional_items}) -> (
        assert (0 = min_items);
        assert (Option.is_none max_items);
        assert (not unique_items);
        assert (Option.is_none additional_items);
        Logs.debug (fun m -> m "process mono-morphic array under '%s'\n"  (Q.json_pointer_of_path ~wildcards:true path));
        let new_path = path @ [`Field "items"] in
        let t' = add_path t ~path:new_path in
        process_element t' ~schema_js ~path:new_path element
      )
    | Combine (c, elements) -> (
        match c with
        | All_of -> (
            Logs.debug (fun m -> m "process combination allOf with %d elements under '%s'\n"  (List.length elements) (Q.json_pointer_of_path ~wildcards:true path));
            elements
              |> List.iteri (fun i el ->
                let new_path = path @ [`Field "allOf"; `Index i] in
                let t' = add_path t ~path:new_path in
                process_element t' ~schema_js ~path:new_path el
              )
          )
        | One_of -> (
            Logs.debug (fun m -> m "process combination oneOf with %d elements under '%s'\n"  (List.length elements) (Q.json_pointer_of_path ~wildcards:true path));
            elements
              |> List.iteri (fun i el ->
                let new_path = path @ [`Field "oneOf"; `Index i] in
                let t' = add_path t ~path:new_path in
                process_element t' ~schema_js ~path:new_path el
              )
          )
        | Any_of -> Logs.debug (fun m -> m "TODO combinator ANY_OF @ %s" (Q.json_pointer_of_path ~wildcards:true path))
        | Not -> let _ = failwith (Printf.sprintf "TODO combinator NOT @ %s" (Q.json_pointer_of_path ~wildcards:true path)) in ()
      )
    | Def_ref ref_path -> (
        Logs.debug (fun m -> m "Dependencies - def_ref: path '%s', ref_path: '%s'" (Q.json_pointer_of_path path) (Q.json_pointer_of_path ref_path));
        process_dfn t ~schema_js ~path:ref_path
      )

    | Id_ref _ -> let _ = failwith "TODO Id_ref" in ()
    | Ext_ref _ -> let _ = failwith "TODO Ext_ref" in ()
    | String _ as _s -> ()
    | Integer _ -> () (* failwith "TODO Integer" *)
    | Number _ -> () (* failwith "TODO Number" *)
    | Boolean -> () (* failwith "TODO Boolean" *)
    | Null -> () (* failwith "TODO Null" *)
    | Any -> () (* failwith "TODO Any" *)
    | Dummy -> let _ = failwith "TODO Dummy" in ()

  and process_property t ~schema_js ~path = function
    (* NOTE can be empty object, so only recurse if len=1 *)
    | EmptyObj _specs ->
        Logs.debug (fun m -> m "process property '%s' under '%s'\n"  "EmptyObject" (Q.json_pointer_of_path ~wildcards:true path))
    | Obj specs -> (
      let (pname, element, _, _) = List. hd specs.properties in
      Logs.debug (fun m -> m "process property '%s' under '%s'\n"  pname (Q.json_pointer_of_path ~wildcards:true path));
      let new_path = path @ [`Field "properties"; `Field pname ] in
      let t' = add_path t ~path:new_path in
      process_element t' ~schema_js ~path:new_path element
    )


  let process schema_js =
    let names = function
      | `O fields -> fields |> List.map (fun (nm, _) -> nm)
      | _ -> []
    in
    let t = Data.create 500 in
    let ns = Q.query [`Field "definitions"] schema_js |> names in
    Logs.debug (fun m -> m "\n\nprocessing '%d' names\n" @@ List.length ns);
    ns |> List.iter (fun nm ->
        let path = [`Field "definitions"; `Field nm]
        in process_dfn t ~schema_js ~path
      );
    match Data.find_opt t _KEY |> Option.value ~default:(SortedModuleNames []) with
    | SortedModuleNames xs -> xs
    | Visited _ -> []


end
