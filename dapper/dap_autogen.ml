module Q = Json_query
module S = Json_schema


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
          | `Star | `Next -> None
          | `Index n -> Some (string_of_int n)
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
  let to_js_ptr t = Q.json_pointer_of_path t.path

  let is_command t =
    String.equal t.safe_name _COMMAND

  let is_event t =
    String.equal t.safe_name _EVENT

end


let _unweird_name nm =
  match (String.lowercase_ascii nm) with
  | "type" | "module" | "lazy" -> Printf.sprintf "%s_" nm
  | "null" -> "empty_"
  | _ -> nm


module Enum_spec = struct
  type t = {
    safe_name: string; (* safe to use as an ocaml enum value *)
    field_name: string; (* original field name as per spec *)
  }

  let make ~field_name () =
    let safe_name =
      _unweird_name field_name
      |> Stringext.replace_all ~pattern:" " ~with_:"_"
      |> String.capitalize_ascii
    in
    {safe_name; field_name}
end


module Prop_spec = struct
  type t = {
    safe_name: string; (* safe to use as an ocaml record name *)
    field_name: string; (* original field name as per spec *)
    field_type: ModuleName.t;
    field_enc:string;
    field_t:string;
    required: bool;
  }

  let make ~field_name ~field_type ?(required=true) () =
    let safe_name =
      _unweird_name field_name
      |> Stringext.replace_all ~pattern:" " ~with_:"_"
    in
    let field_enc = "" in
    let field_t = "" in
    {safe_name; field_name; field_type; field_enc; field_t; required}

end

type 'a maybe_spec = [ `Required of 'a | `Optional of 'a | `None]


module RequestSpec = struct

  type t = {
    module_name: ModuleName.t;
    command_value: string;
    args_name: ModuleName.t maybe_spec;
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
    module_name: ModuleName.t;
    command_value: string;
    body_name: ModuleName.t maybe_spec;
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
    module_name: ModuleName.t;
    event_value: string;
    body_name: ModuleName.t maybe_spec;
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



module LeafNodes = struct

  type specs = {
    encoder: string
  }

  type enum_specs = {
    field_name: ModuleName.t;
    enums: Enum_spec.t list;
  }
  type empty_object_specs = {
    field_name: ModuleName.t;
  }
  type object_specs = {
    field_name: ModuleName.t;
    fields: Prop_spec.t list;
  }
  type array_specs = {
    field_name: ModuleName.t;
    inner_type: ModuleName.t;
  }

  type name_type_specs = {
    field_name: ModuleName.t;
    field_type: ModuleName.t;
  }

  type t = [
    | `Json
    | `Field of specs (* final string, int, number etc *)
    | `Enum of enum_specs
    | `EnumSuggestions of enum_specs
    | `EmptyObject of empty_object_specs
    | `Object of object_specs
    (* | `CyclicObject of object_specs
     * | `LargeObject of object_specs *)
    | `Array of array_specs
    | `Ref of name_type_specs * bool
    | `AllOf of name_type_specs list
    | `OneOf of name_type_specs list
    (* | `Request of RequestSpec.t
     * | `Response of ResponseSpec.t
     * | `Event of EventSpec.t *)
  ]

end

module Dfs = struct

  module Data = Hashtbl.Make(struct type t = string let equal = String.equal let hash = Hashtbl.hash end)

  let _KEY = "sorted_module_names"
  let _VISITED = "visited"
  let _EL = "elements"

  type visit_status = Started | Finished | Unknown

  type el =
    | SortedModuleNames of (ModuleName.t*string) list (* under _KEY *)
    | Visited of visit_status Data.t (* under _VISITED *)
    | Leaves of LeafNodes.t Data.t (* under _EL *)
    | Command of LeafNodes.enum_specs (* under _COMMAND *)
    | Event of LeafNodes.enum_specs (* under _EVENT *)

  type t = el Data.t

  let add_sorted_module_name t ~path ~desc =
    (* adds to the _KEY list for topo sort of all names, *)
    let empty = [] in
    let mname = ModuleName.of_path ~path in
    let module_names = match (Data.find_opt t _KEY |> Option.value ~default:(SortedModuleNames empty)) with
      | SortedModuleNames names -> (mname, desc) :: names
      | Visited _ -> empty
      | Leaves _ -> empty
      | Command _ -> empty
      | Event _ -> empty
    in
    Data.replace t _KEY (SortedModuleNames module_names)

  let check_and_add_visited t ~path =
    (* add to the visited tbl if not already there,
       returns whether a new one was added,
       dfn that result in a Command or Event get added under their full path *)
    let dfn = Q.json_pointer_of_path path in
    let empty = Data.create 500 in
    let added_new, visited = match (Data.find_opt t _VISITED |> Option.value ~default:(Visited empty)) with
      | SortedModuleNames _ -> (false, empty)
      | Visited vs ->
        let is_present = Data.mem vs dfn in
        if not is_present then (
          Data.replace vs dfn Started;
          (true, vs)
        ) else (false, vs)
      | Leaves _ -> (false, empty)
      | Command _ -> (false, empty)
      | Event _ -> (false, empty)
    in
    Data.replace t _VISITED (Visited visited);
    added_new

  let assert_and_close_visited t ~path =
    (* asserts is in the visited tbl
       and then sets its visit_status to Finished *)
    let dfn = Q.json_pointer_of_path path in
    let empty = Data.create 500 in
    let visited = match (Data.find_opt t _VISITED |> Option.value ~default:(Visited empty)) with
      | SortedModuleNames _ -> empty
      | Visited vs ->
        assert (Data.mem vs dfn);
        Data.replace vs dfn Finished;
        vs
      | Leaves _ -> empty
      | Command _ -> empty
      | Event _ -> empty
    in
    Data.replace t _VISITED (Visited visited)

  let get_visit_status t ~path =
    let dfn = Q.json_pointer_of_path path in
    let empty = Data.create 500 in
    match (Data.find_opt t _VISITED |> Option.value ~default:(Visited empty)) with
      | SortedModuleNames _ -> Unknown
      | Visited vs ->
        Data.find_opt vs dfn |> Option.value ~default:Unknown
      | Leaves _ -> Unknown
      | Command _ -> Unknown
      | Event _ -> Unknown


  let add_leaf_node t ~path ~leaf =
    let empty = Data.create 500 in
    let ls = match (Data.find_opt t _EL |> Option.value ~default:(Leaves empty)) with
      | SortedModuleNames _ -> empty
      | Visited _ -> empty
      | Leaves ls -> Data.replace ls (Q.json_pointer_of_path path) leaf; ls
      | Command _ -> empty
      | Event _ -> empty
    in
    Data.replace t _EL (Leaves ls)

  let append_to_command_enum_node t ~enums =
    let open LeafNodes in
    let empty : enum_specs = {
      field_name=(ModuleName.of_string ~js_ptr:("/"^_COMMAND));
      enums=[];
    } in

    let xs = match (Data.find_opt t _COMMAND |> Option.value ~default:(Command empty)) with
      | SortedModuleNames _ -> empty
      | Visited _ -> empty
      | Leaves _ -> empty
      | Command es -> {es with enums=es.enums @ enums}
      | Event _ -> empty
    in
    Data.replace t _COMMAND (Command xs)

  let append_to_event_enum_node t ~enums =
    let open LeafNodes in
    let empty : enum_specs = {
      field_name=(ModuleName.of_string ~js_ptr:("/"^_EVENT));
      enums=[];
    } in

    let xs = match (Data.find_opt t _EVENT |> Option.value ~default:(Event empty)) with
      | SortedModuleNames _ -> empty
      | Visited _ -> empty
      | Leaves _ -> empty
      | Command _ -> empty
      | Event es -> {es with enums=es.enums @ enums}
    in
    Data.replace t _EVENT (Event xs)

  let is_special_definition ~path =
    let pth = Array.of_list path in
    let n = Array.length pth in
    if n = 2 && pth.(0) = `Field "definitions" then
      match pth.(1) with
      | `Field "ProtocolMessage" | `Field "Request" | `Field "Event" | `Field "Response" -> true
      | _ -> false
    else
      false

  let rec process_definition t ~schema_js ~path =
    (* this will be a *Request/*Response/*Event/Object top level definition
       will want to ignore base class ProtocolMessage, Request, Response, Event types
       as they are handled with the functors above
       only want to recurse down into the defn if it is not already being/been visited *)
    let dfn = Q.json_pointer_of_path path in

    Logs.debug (fun m -> m "process dfn start: '%s'"  dfn);
    if is_special_definition ~path then
      Logs.debug (fun m -> m "!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!special case: '%s'"  dfn)
    else (
        (* check is valid name by finding the definition *)
        let schema = Json_schema.of_json schema_js in
        let element = S.find_definition dfn schema in
        (* if added new one then recurse too *)
        if check_and_add_visited t ~path then (
          Logs.debug (fun m -> m "visiting: '%s'"  dfn);
          process_element t ~schema_js ~path element;
          assert_and_close_visited t ~path;
          Logs.debug (fun m -> m "finished: '%s'"  dfn)
        ) else (
          Logs.debug (fun m -> m "already visiting/visited: '%s'"  dfn)
        )
      );
    Logs.debug (fun m -> m "process dfn end: '%s'"  dfn)


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
      let field_name = ModuleName.of_path ~path in
      let enums = enums |> List.map (fun field_name -> Enum_spec.make ~field_name ()) in
      if ModuleName.is_command field_name then (
        append_to_command_enum_node t ~enums
        (* NOTE dont add to topo sort list *)
      )
      else if ModuleName.is_event field_name then (
        append_to_event_enum_node t ~enums
        (* NOTE dont add to topo sort list *)
      )
      else if n > 1 then (
        let leaf = LeafNodes.(`Enum {field_name; enums}) in
        add_leaf_node t ~path ~leaf;
        add_sorted_module_name t ~path ~desc:"enum"
      )
      else (
        Logs.debug (fun m -> m "Ignoring element under '%s'"
                       (Q.json_pointer_of_path ~wildcards:true path))
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
        if List.length properties = 0 then
          (* TODO append EmptyObject, dont need to call process_property  *)
          let leaf = LeafNodes.(`EmptyObject {field_name=ModuleName.of_path ~path}) in
          add_leaf_node t ~path ~leaf;
          add_sorted_module_name t ~path ~desc:"empty object";
        else (
          properties
          |> List.iter (fun (pname, ty, _required, _extra) ->
              Logs.debug (fun m -> m "process property '%s' under '%s'"  pname (Q.json_pointer_of_path ~wildcards:true path));
              let new_path = path @ [`Field "properties"; `Field pname ] in
              process_element t ~schema_js ~path:new_path ty
            );

          (* should now be able to make the object specs *)
          let fields =
            properties
            |> List.map (fun (pname, _ty, required, _extra) ->
                let prop_path = path @ [`Field "properties"; `Field pname ] in
                let field_type = ModuleName.of_path ~path:prop_path in
                Prop_spec.make ~field_name:pname ~field_type ~required ())
          in
          let leaf = LeafNodes.(`Object {field_name=(ModuleName.of_path ~path); fields}) in
          add_leaf_node t ~path ~leaf;
          add_sorted_module_name t ~path ~desc:"object";
        )

      )
    | Array (_, _) -> let _ = failwith "TODO array" in ()
    | Monomorphic_array (element, {min_items; max_items; unique_items; additional_items}) -> (
        assert (0 = min_items);
        assert (Option.is_none max_items);
        assert (not unique_items);
        assert (Option.is_none additional_items);
        Logs.debug (fun m -> m "process mono-morphic array under '%s'"  (Q.json_pointer_of_path ~wildcards:true path));
        let new_path = path @ [`Field "items"] in
        process_element t ~schema_js ~path:new_path element;
        (* TODO should now be able to make the array specs *)
        let field_name = ModuleName.of_path ~path in
        let inner_type = ModuleName.of_path ~path:new_path in
        let leaf = LeafNodes.(`Array {field_name; inner_type}) in
        add_leaf_node t ~path ~leaf;
        add_sorted_module_name t ~path ~desc:"array";
      )
    | Combine (c, elements) -> (
        match c with
        | All_of -> (
            Logs.debug (fun m -> m "process combination allOf with %d elements under '%s'"  (List.length elements) (Q.json_pointer_of_path ~wildcards:true path));
            elements
            |> List.iteri (fun i el ->
                let new_path = path @ [`Field "allOf"; `Index i] in
                process_element t ~schema_js ~path:new_path el;
              );
            (* make an all_of thingy after filter out Request/Response/Event/ProtocolMessage refs  *)
            let nts =
              elements
              |> List.mapi (fun i _el ->
                  let new_path = path @ [`Field "allOf"; `Index i] in
                  let field_name = ModuleName.of_path ~path in
                  let field_type = ModuleName.of_path ~path:new_path in
                  ({field_name; field_type} : LeafNodes.name_type_specs)
                )
            in
            let leaf = `AllOf nts in
            add_leaf_node t ~path ~leaf;
            add_sorted_module_name t ~path ~desc:"allof";
          )
        | One_of -> (
            Logs.debug (fun m -> m "process combination oneOf with %d elements under '%s'"  (List.length elements) (Q.json_pointer_of_path ~wildcards:true path));
            elements
            |> List.iteri (fun i el ->
                let new_path = path @ [`Field "oneOf"; `Index i] in
                process_element t ~schema_js ~path:new_path el;
              );
            let nts =
              elements
              |> List.mapi (fun i _el ->
                  let new_path = path @ [`Field "oneOf"; `Index i] in
                  let field_name = ModuleName.of_path ~path in
                  let field_type = ModuleName.of_path ~path:new_path in
                  ({field_name; field_type} : LeafNodes.name_type_specs)
                )
            in
            let leaf = `OneOf nts in
            add_leaf_node t ~path ~leaf;
            add_sorted_module_name t ~path ~desc:"oneof";
          )
        | Any_of -> (
            Logs.debug (fun m -> m "TODO combinator ANY_OF @ %s with %d choices" (Q.json_pointer_of_path ~wildcards:true path) (List.length elements));
            let names = elements
            |> List.map (fun (el:S.element) -> match el.kind with
                | Monomorphic_array _ -> "array"
                | Boolean -> "boolean"
                | Integer _ -> "integer"
                | Null -> "null"
                | Number _ -> "number"
                | Object _ -> "object"
                | String _ -> "string"
                | _ -> failwith "unknown AnyOf name"
              )
            |> List.sort (String.compare)
            in
            (* TODO be better *)
            let leaf : LeafNodes.t =
              match (String.concat "," names) with
              | "array,boolean,integer,null,number,object,string" -> `Json
              | _ -> `Field {encoder="string"}
            in
            add_leaf_node t ~path ~leaf;
            add_sorted_module_name t ~path ~desc:"anyof";
          )
        | Not -> let _ = failwith (Printf.sprintf "TODO combinator NOT @ %s" (Q.json_pointer_of_path ~wildcards:true path)) in ()
      )
    | Def_ref ref_path -> (
        let ref_path_str = Q.json_pointer_of_path ref_path in
        Logs.debug (fun m -> m "Dependencies - def_ref: path '%s', ref_path: '%s'" (Q.json_pointer_of_path path) ref_path_str);
        let is_cyclic = match get_visit_status t ~path:ref_path with
        | Started -> Logs.info (fun m -> m "1) Cyclic dependency - def_ref: path '%s', ref_path: '%s'" (Q.json_pointer_of_path path) ref_path_str); true
        | _ -> false
        in

        process_definition t ~schema_js ~path:ref_path;
        let field_name = ModuleName.of_path ~path in
        let field_type = ModuleName.of_path ~path:ref_path in
        let leaf = LeafNodes.(`Ref ({field_name; field_type}, is_cyclic)) in
        add_leaf_node t ~path ~leaf;
        add_sorted_module_name t ~path ~desc:(Printf.sprintf "ref%s" (if is_cyclic then " - cyclic" else ""))
      )

    | Id_ref _ -> let _ = failwith "TODO Id_ref" in ()
    | Ext_ref _ -> let _ = failwith "TODO Ext_ref" in ()
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
      let leaf, names =
        try
          let enum_path = path @ [`Field "_enum"] in
          match Q.query enum_path schema_js with
          | `A names ->
            let names =
              List.map Ezjsonm.decode_string_exn names
            in
            let field_name = ModuleName.of_path ~path in
            let enums = names |> List.map (fun nm -> Enum_spec.make ~field_name:nm () ) in
            Some LeafNodes.(`EnumSuggestions {field_name; enums}), Some (String.concat ", " names)
          | _ -> None, None
        with _ -> None, None
      in
      let leaf =
        leaf
        |> Option.value ~default:LeafNodes.(`Field {encoder="string"})
      in
      let desc =
        names
        |> Option.map (fun nms -> (Printf.sprintf "string field with suggestions {%s}" nms))
        |> Option.value ~default:"string field"
      in
      add_leaf_node t ~path ~leaf;
      add_sorted_module_name t ~path ~desc;

    | Integer _ ->
      add_leaf_node t ~path ~leaf:LeafNodes.(`Field {encoder="int64"});
      add_sorted_module_name t ~path ~desc:"int field";
    | Number _ ->
      add_leaf_node t ~path ~leaf:LeafNodes.(`Field {encoder="int64"});
      add_sorted_module_name t ~path ~desc:"number field";
    | Boolean ->
      add_leaf_node t ~path ~leaf:LeafNodes.(`Field {encoder="bool"});
      add_sorted_module_name t ~path ~desc:"bool field";
    | Null -> let _ = failwith "TODO Null" in ()
    | Any -> let _ = failwith "TODO Any" in ()
    | Dummy -> let _ = failwith "TODO Dummy" in ()

  let make ~schema_js =
    let names = function
      | `O fields -> fields |> List.map (fun (nm, _) -> nm)
      | _ -> []
    in
    let t = Data.create 500 in
    (* NOTE know all /definitions/ are objects and are the only top-level things *)
    let ns = Q.query [`Field "definitions"] schema_js |> names in
    Logs.debug (fun m -> m "\n\nprocessing '%d' names" @@ List.length ns);
    ns |> List.iter (fun nm ->
        let path = [`Field "definitions"; `Field nm]
        in process_definition t ~schema_js ~path
      );
    t

  let _get t ~(what:[ `Module_names | `Elements | `Command_enum | `Event_enum ]) =
    let ky = match what with
      | `Module_names -> _KEY
      | `Elements -> _EL
      | `Command_enum -> _COMMAND
      | `Event_enum -> _EVENT
    in
    Data.find t ky

  let topo_sorted t =
    match _get t ~what:`Module_names with
    | SortedModuleNames xs -> xs |> List.rev (* |> List.map fst *)
    | _ -> []

  let get_enums =
    let get_enum_suggestions t =
      match _get t ~what:`Elements with
      | Leaves tbl ->
        tbl
        |> Data.to_seq
        |> Seq.filter_map (fun (_ky, el) -> match el with | `EnumSuggestions specs -> Some (`EnumSuggestions specs) | _ -> None)
        |> List.of_seq
      | _ -> []
    in
    function t ->
      let enum_suggestions = get_enum_suggestions t in

      let pmsg : LeafNodes.enum_specs = {
        field_name=ModuleName.of_string ~js_ptr:"/ProtocolMessage_type";
        enums=["request"; "response"; "event"] |> List.map (fun field_name -> Enum_spec.make ~field_name ());
      }
      in
      let command_enum = match _get t ~what:`Command_enum with | Command specs -> specs | _ -> failwith "no Command enum found" in
      let event_enum = match _get t ~what:`Event_enum with | Event specs -> specs | _ -> failwith "no Event enum found" in
      let enums = match _get t ~what:`Elements with
        | Leaves tbl ->
          tbl
          |> Data.to_seq
          |> Seq.filter_map (fun (_ky, el) -> match el with | `Enum specs -> Some (`Enum specs) | _ -> None)
          |> List.of_seq
        | _ -> []
      in
      (`Enum pmsg) :: (`Enum command_enum) :: (`Enum event_enum) :: enums @ enum_suggestions

  let get_objects =
    function t ->
      let mnames = topo_sorted t |> List.map fst in
      let tbl = match _get t ~what:`Elements with | Leaves ls -> ls | _ -> failwith "cannot find any elements" in
      mnames |> List.filter_map (fun mname ->
          let leaf = Data.find_opt tbl (ModuleName.to_js_ptr mname) in
          Option.bind leaf (function | `Object specs -> Some (`Object specs) | _ -> None ) (* `EmptyObject specs -> Some (`EmptyObject specs) | _ -> None) *)
        )




end
