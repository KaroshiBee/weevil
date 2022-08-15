open Json_schema
module Q = Json_query


module StrHashtbl = Hashtbl.Make(struct type t = string let equal = String.equal let hash = Hashtbl.hash end)

(* module Dependencies = struct
 *
 *   (\* TODO thread these through as module state *\)
 *   let tbl : string list StrHashtbl.t = StrHashtbl.create 100
 *
 *   (\* extract all $ref *\)
 *   let rec process_name ~schema name =
 *     Logs.debug (fun m -> m "process name start: '%s'\n"  name);
 *
 *     (\* first check is valid name *\)
 *     let _ = Q.path_of_json_pointer name in
 *     let element = find_definition name schema in
 *     if not @@ StrHashtbl.mem tbl name then StrHashtbl.add tbl name [];
 *     process_element ~schema ~name element;
 *
 *     Logs.debug (fun m -> m "process name end: '%s'\n"  name);
 *
 *   and process_element ~schema ~name el =
 *     Logs.debug (fun m -> m "process element under '%s'\n"  name);
 *     process_kind ~schema ~name el.kind
 *
 *   and process_kind ~schema ~name = function
 *     | Object {properties; pattern_properties; additional_properties; min_properties; max_properties; schema_dependencies; property_dependencies} -> (
 *         assert (0 = List.length pattern_properties);
 *         assert (0 = List.length schema_dependencies);
 *         assert (0 = List.length property_dependencies);
 *         assert (0 = min_properties);
 *         assert (Option.is_none max_properties);
 *         assert (Option.is_some additional_properties);
 *         Logs.debug (fun m -> m "process object with %d properties under '%s'\n"  (List.length properties) name);
 *         properties |> List.iter (fun (pname, ty, required, extra) -> process_property ~schema ~name pname ty required extra)
 *       )
 *     | Array (_, _) -> () (\* failwith "TODO array" *\)
 *     | Monomorphic_array (element, {min_items; max_items; unique_items; additional_items}) -> (
 *         assert (0 = min_items);
 *         assert (Option.is_none max_items);
 *         assert (not unique_items);
 *         assert (Option.is_none additional_items);
 *         Logs.debug (fun m -> m "process mono-morphic array under '%s'\n"  name);
 *         process_element ~schema ~name element
 *       )
 *     | Combine (c, elements) -> (
 *         match c with
 *         | All_of -> (
 *             Logs.debug (fun m -> m "process combination with %d elements under '%s'\n"  (List.length elements) name);
 *             elements |> List.iter (fun el -> process_element ~schema ~name el)
 *           )
 *         | Any_of | One_of | Not -> () (\* failwith "TODO other combinators" *\)
 *       )
 *     | Def_ref path ->
 *       let path_str = Q.json_pointer_of_path path in
 *       (\* add path_str to the entries under name - but only if not already there *\)
 *       let ps = StrHashtbl.find_opt tbl name |> Option.value ~default:[] in
 *       if not @@ List.mem path_str ps then
 *         StrHashtbl.replace tbl name (path_str :: ps);
 *       (\* if path_str is also a new ref then recurse into it *\)
 *       if not @@ StrHashtbl.mem tbl path_str then (
 *         Logs.debug (fun m -> m "found new $ref '%s' under '%s', recursing\n"  path_str name);
 *         process_name ~schema path_str
 *       )
 *       else (
 *         Logs.debug (fun m -> m "found old $ref '%s' under '%s', not recursing\n"  path_str name);
 *       )
 *     | Id_ref _ -> () (\* failwith "TODO Id_ref" *\)
 *     | Ext_ref _ -> () (\* failwith "TODO Ext_ref" *\)
 *     | String _ -> () (\* failwith "TODO String" *\)
 *     | Integer _ -> () (\* failwith "TODO Integer" *\)
 *     | Number _ -> () (\* failwith "TODO Number" *\)
 *     | Boolean -> () (\* failwith "TODO Boolean" *\)
 *     | Null -> () (\* failwith "TODO Null" *\)
 *     | Any -> () (\* failwith "TODO Any" *\)
 *     | Dummy -> () (\* failwith "TODO Dummy" *\)
 *
 *   and process_property ~schema ~name pname element _required _extra =
 *     Logs.debug (fun m -> m "process property '%s' under '%s'\n"  pname name);
 *     process_element ~schema ~name element
 *
 *
 *   let names = function
 *     | `O fields -> fields |> List.map (fun (nm, _) -> nm)
 *     | _ -> []
 *
 *
 *   let process schema =
 *     StrHashtbl.reset tbl;
 *     let ns = Q.query [`Field "definitions"] (to_json schema) |> names in
 *     Logs.debug (fun m -> m "\n\nprocessing '%d' names\n" @@ List.length ns);
 *     ns |> List.iter (fun nm -> let name = Printf.sprintf "/definitions/%s" nm in process_name ~schema name);
 *     tbl
 *
 *   let pp_tbl tbl =
 *     StrHashtbl.to_seq tbl
 *     |> List.of_seq
 *     |> List.map (fun (name, deps) -> Printf.sprintf "%s:\n  [ %s ]" name @@ String.concat "; " deps)
 *     |> List.sort String.compare
 *     |> List.iter (fun ln -> Logs.debug (fun m -> m "%s\n\n" ln))
 *
 * end
 *
 *
 * (\*
 * the _enum fields arent picked up by Json_schema module
 * and so using Json_schema.to_json wont work.
 * Have to use Ezjsonm.from_channel to read the raw json and then query that.
 * It is for this reason that also have to manually add some stuff to a path as
 * recursion progresses (e.g. "properties" or "allOf" > index 1)
 * *\)
 *
 * module Enums = struct
 *
 *   let make_module_name ~path =
 *     let name =
 *       path
 *       |> List.filter_map (fun el ->
 *           match el with
 *           | `Star | `Index _ | `Next -> None
 *           | `Field f -> (
 *               match f with
 *               | "definitions" | "allOf" | "items" | "_enum" | "properties" -> None
 *               | _ -> Some f
 *             )
 *           | _ -> None
 *         )
 *       |> String.concat "_"
 *     in
 *     String.capitalize_ascii name
 *     (\* String.(lowercase_ascii name |> capitalize_ascii) *\)
 *
 *   (\* TODO thread these through as module state *\)
 *   let tbl : string list StrHashtbl.t = StrHashtbl.create 100
 *
 *   let rec process_dfn ~schema_js ~path =
 *     let dfn = Q.json_pointer_of_path ~wildcards:true path in
 *     Logs.debug (fun m -> m "process dfn start: '%s'\n"  dfn);
 *
 *     (\* first check is valid name *\)
 *     let schema = Json_schema.of_json schema_js in
 *     let element = find_definition dfn schema in
 *     (\* if not @@ StrHashtbl.mem tbl dfn then StrHashtbl.add tbl dfn []; *\)
 *     process_element ~schema_js ~path element;
 *
 *     Logs.debug (fun m -> m "process dfn end: '%s'\n"  dfn);
 *
 *   and process_element ~schema_js ~path el =
 *     Logs.debug (fun m -> m "process element under '%s'\n"  (Q.json_pointer_of_path ~wildcards:true path));
 *     process_kind ~schema_js ~path el.kind
 *
 *   and process_kind ~schema_js ~path = function
 *     | Object {properties; pattern_properties; additional_properties; min_properties; max_properties; schema_dependencies; property_dependencies} -> (
 *         assert (0 = List.length pattern_properties);
 *         assert (0 = List.length schema_dependencies);
 *         assert (0 = List.length property_dependencies);
 *         assert (0 = min_properties);
 *         assert (Option.is_none max_properties);
 *         assert (Option.is_some additional_properties);
 *         Logs.debug (fun m -> m "process object with %d properties under '%s'\n"  (List.length properties) (Q.json_pointer_of_path ~wildcards:true path));
 *         properties |> List.iter (fun (pname, ty, required, extra) -> process_property ~schema_js ~path pname ty required extra)
 *       )
 *     | Array (_, _) -> () (\* failwith "TODO array" *\)
 *     | Monomorphic_array (element, {min_items; max_items; unique_items; additional_items}) -> (
 *         assert (0 = min_items);
 *         assert (Option.is_none max_items);
 *         assert (not unique_items);
 *         assert (Option.is_none additional_items);
 *         Logs.debug (fun m -> m "process mono-morphic array under '%s'\n"  (Q.json_pointer_of_path ~wildcards:true path));
 *         let new_path = path @ [`Field "items"] in
 *         process_element ~schema_js ~path:new_path element
 *       )
 *     | Combine (c, elements) -> (
 *         match c with
 *         | All_of -> (
 *             Logs.debug (fun m -> m "process combination with %d elements under '%s'\n"  (List.length elements) (Q.json_pointer_of_path ~wildcards:true path));
 *             let new_path = path @ [`Field "allOf"; `Index 1] in
 *             elements |> List.iter (fun el -> process_element ~schema_js ~path:new_path el)
 *           )
 *         | Any_of | One_of | Not -> () (\* failwith "TODO other combinators" *\)
 *       )
 *     | Def_ref _ -> ()
 *     | Id_ref _ -> () (\* failwith "TODO Id_ref" *\)
 *     | Ext_ref _ -> () (\* failwith "TODO Ext_ref" *\)
 *     | String _ -> process_string ~schema_js ~path
 *     | Integer _ -> () (\* failwith "TODO Integer" *\)
 *     | Number _ -> () (\* failwith "TODO Number" *\)
 *     | Boolean -> () (\* failwith "TODO Boolean" *\)
 *     | Null -> () (\* failwith "TODO Null" *\)
 *     | Any -> () (\* failwith "TODO Any" *\)
 *     | Dummy -> () (\* failwith "TODO Dummy" *\)
 *
 *   and process_property ~schema_js ~path pname element _required _extra =
 *     Logs.debug (fun m -> m "process property '%s' under '%s'\n"  pname (Q.json_pointer_of_path ~wildcards:true path));
 *     let new_path = path @ [`Field "properties"; `Field pname ] in
 *     process_element ~schema_js ~path:new_path element
 *
 *   and process_string ~schema_js ~path =
 *     Logs.debug (fun m -> m "got string under '%s'\n"  (Q.json_pointer_of_path ~wildcards:true path));
 *     let aux ~path ~field =
 *       try
 *         let enum_path = path @ [`Field field] in
 *         match Q.query enum_path schema_js with
 *         | `A names ->
 *           let names =
 *             List.map Ezjsonm.decode_string_exn names
 *           in
 *           let module_name = match List.rev path with
 *           | `Field "command" :: _ -> "Command_enum" (\* Request command types *\)
 *           | `Field "event" :: _ -> "Event_enum" (\* Event types *\)
 *           (\* | `Field "message" :: _ ->
 *            *   if field = "_enum" && List.length names = 1 && List.hd names = "cancelled" then
 *            *     "Message_enum" (\\* Response message types - only one currently *\\)
 *            *   else
 *            *     make_module_name ~path:enum_path *\)
 *           | _ -> make_module_name ~path:enum_path
 *           in
 *           let xs = StrHashtbl.find_opt tbl module_name |> Option.value ~default:[] in
 *           StrHashtbl.replace tbl module_name (names @ xs);
 *         | _ -> ()
 *       with _ -> ()
 *     in
 *     aux ~path ~field:"_enum";
 *     aux ~path ~field:"enum"
 *
 *
 *   let names = function
 *     | `O fields -> fields |> List.map (fun (nm, _) -> nm)
 *     | _ -> []
 *
 *
 *   let process schema_js =
 *     StrHashtbl.reset tbl;
 *     let ns = Q.query [`Field "definitions"] schema_js |> names in
 *     Logs.debug (fun m -> m "\n\nprocessing '%d' names\n" @@ List.length ns);
 *     ns |> List.iter (fun nm -> let path = [`Field "definitions"; `Field nm] in process_dfn ~schema_js ~path);
 *     tbl
 *
 *   let pp_tbl tbl =
 *     StrHashtbl.to_seq tbl
 *     |> List.of_seq
 *     |> List.map (fun (name, deps) -> Printf.sprintf "%s:\n  [ %s ]" name @@ String.concat "; " deps)
 *     |> List.sort String.compare
 *     |> List.iter (fun ln -> Logs.debug (fun m -> m "%s\n\n" ln))
 *
 * end
 *
 *
 * module GenEncodings = struct
 *   let clean_field_name field =
 *     Stringext.replace_all field ~pattern:" " ~with_:"_" |> String.capitalize_ascii
 *
 *   let struct_tpl ~name ~body =
 *     Printf.sprintf "\n(\* WARN autogenerated - do not modify by hand *\)\n\nmodule %s = struct\n%s\nend" name body
 *
 *   let typet_tpl ~fields =
 *     let s = fields
 *             |> List.map clean_field_name
 *             |> String.concat " | "
 *     in
 *     Printf.sprintf "type t = | %s" s
 *
 *   let enc_s_t_tpl ~fields ~name =
 *     let s = fields
 *       |> List.map (fun el -> Printf.sprintf "\"%s\" -> %s" el (clean_field_name el))
 *       |> String.concat " | "
 *     in
 *     Printf.sprintf "(function | %s | _ -> failwith \"Unknown %s\")" s name
 *
 *   let enc_t_s_tpl ~fields =
 *     let s = fields
 *             |> List.map (fun el -> Printf.sprintf "%s -> \"%s\"" (clean_field_name el) el)
 *             |> String.concat " | "
 *     in
 *     Printf.sprintf "(function | %s)" s
 *
 *   let enc_tpl ~fields ~name = [
 *     "let enc = "; "let open Data_encoding in";
 *     "conv";
 *     enc_t_s_tpl ~fields;
 *     enc_s_t_tpl ~fields ~name;
 *     "string";
 *   ] |> String.concat "\n"
 *
 *
 *   let enum_tpl name fields =
 *     if List.length fields = 1 then (
 *       Logs.debug (fun m -> m "Ignoring %s, only one field\n" name); ""
 *     )
 *     else
 *       let body = [
 *         typet_tpl ~fields;
 *         enc_tpl ~fields ~name;
 *       ] |> String.concat "\n"
 *       in struct_tpl ~name ~body
 *
 *
 *
 * end *)


module WalkSchema = struct

  type 'a t = {
    strings: 'a StrHashtbl.t -> Json_repr.ezjsonm -> Json_query.path -> unit;
  }

  let make ~strings =
    {strings}


  let rec process_dfn t ~tbl ~schema_js ~path =
    let dfn = Q.json_pointer_of_path ~wildcards:true path in
    Logs.debug (fun m -> m "process dfn start: '%s'\n"  dfn);

    (* first check is valid name *)
    let schema = Json_schema.of_json schema_js in
    let element = find_definition dfn schema in
    (* if not @@ StrHashtbl.mem tbl dfn then StrHashtbl.add tbl dfn []; *)
    process_element t ~tbl ~schema_js ~path element;

    Logs.debug (fun m -> m "process dfn end: '%s'\n"  dfn);

  and process_element t ~tbl ~schema_js ~path el =
    Logs.debug (fun m -> m "process element under '%s'\n"  (Q.json_pointer_of_path ~wildcards:true path));
    process_kind t ~tbl ~schema_js ~path el.kind

  and process_kind t ~tbl ~schema_js ~path = function
    | Object {properties; pattern_properties; additional_properties; min_properties; max_properties; schema_dependencies; property_dependencies} -> (
        assert (0 = List.length pattern_properties);
        assert (0 = List.length schema_dependencies);
        assert (0 = List.length property_dependencies);
        assert (0 = min_properties);
        assert (Option.is_none max_properties);
        assert (Option.is_some additional_properties);
        Logs.debug (fun m -> m "process object with %d properties under '%s'\n"  (List.length properties) (Q.json_pointer_of_path ~wildcards:true path));
        properties |> List.iter (fun (pname, ty, required, extra) -> process_property t ~tbl ~schema_js ~path pname ty required extra)
      )
    | Array (_, _) -> () (* failwith "TODO array" *)
    | Monomorphic_array (element, {min_items; max_items; unique_items; additional_items}) -> (
        assert (0 = min_items);
        assert (Option.is_none max_items);
        assert (not unique_items);
        assert (Option.is_none additional_items);
        Logs.debug (fun m -> m "process mono-morphic array under '%s'\n"  (Q.json_pointer_of_path ~wildcards:true path));
        let new_path = path @ [`Field "items"] in
        process_element t ~tbl ~schema_js ~path:new_path element
      )
    | Combine (c, elements) -> (
        match c with
        | All_of -> (
            Logs.debug (fun m -> m "process combination with %d elements under '%s'\n"  (List.length elements) (Q.json_pointer_of_path ~wildcards:true path));
            let new_path = path @ [`Field "allOf"; `Index 1] in
            elements |> List.iter (fun el -> process_element t ~tbl ~schema_js ~path:new_path el)
          )
        | Any_of | One_of | Not -> () (* failwith "TODO other combinators" *)
      )
    | Def_ref _ -> ()
    | Id_ref _ -> () (* failwith "TODO Id_ref" *)
    | Ext_ref _ -> () (* failwith "TODO Ext_ref" *)
    | String _ -> process_string t ~tbl ~schema_js ~path
    | Integer _ -> () (* failwith "TODO Integer" *)
    | Number _ -> () (* failwith "TODO Number" *)
    | Boolean -> () (* failwith "TODO Boolean" *)
    | Null -> () (* failwith "TODO Null" *)
    | Any -> () (* failwith "TODO Any" *)
    | Dummy -> () (* failwith "TODO Dummy" *)

  and process_property t ~tbl ~schema_js ~path pname element _required _extra =
    Logs.debug (fun m -> m "process property '%s' under '%s'\n"  pname (Q.json_pointer_of_path ~wildcards:true path));
    let new_path = path @ [`Field "properties"; `Field pname ] in
    process_element t ~tbl ~schema_js ~path:new_path element

  and process_string t ~tbl ~schema_js ~path = t.strings tbl schema_js path


  let process t schema_js =
    let names = function
      | `O fields -> fields |> List.map (fun (nm, _) -> nm)
      | _ -> []
    in
    let tbl : string list StrHashtbl.t = StrHashtbl.create 100 in
    let ns = Q.query [`Field "definitions"] schema_js |> names in
    Logs.debug (fun m -> m "\n\nprocessing '%d' names\n" @@ List.length ns);
    ns |> List.iter (fun nm ->
        let path = [`Field "definitions"; `Field nm]
        in process_dfn t ~tbl ~schema_js ~path
      );
    tbl

end


module GenEncodings = struct

  let make_module_name ~path =
    let name =
      path
      |> List.filter_map (fun el ->
          match el with
          | `Star | `Index _ | `Next -> None
          | `Field f -> (
              match f with
              | "definitions" | "allOf" | "items" | "_enum" | "properties" -> None
              | _ -> Some f
            )
          | _ -> None
        )
      |> String.concat "_"
    in
    String.capitalize_ascii name

  let clean_field_name field =
    Stringext.replace_all field ~pattern:" " ~with_:"_" |> String.capitalize_ascii

  let struct_tpl ~name ~body =
    Printf.sprintf "\n(* WARN autogenerated - do not modify by hand *)\n\nmodule %s = struct\n%s\nend" name body

  let typet_tpl ~fields =
    let s = fields
            |> List.map clean_field_name
            |> String.concat " | "
    in
    Printf.sprintf "type t = | %s" s

  let enc_s_t_tpl ~fields ~name =
    let s = fields
      |> List.map (fun el -> Printf.sprintf "\"%s\" -> %s" el (clean_field_name el))
      |> String.concat " | "
    in
    Printf.sprintf "(function | %s | _ -> failwith \"Unknown %s\")" s name

  let enc_t_s_tpl ~fields =
    let s = fields
            |> List.map (fun el -> Printf.sprintf "%s -> \"%s\"" (clean_field_name el) el)
            |> String.concat " | "
    in
    Printf.sprintf "(function | %s)" s

  let enc_tpl ~fields ~name = [
    "let enc = "; "let open Data_encoding in";
    "conv";
    enc_t_s_tpl ~fields;
    enc_s_t_tpl ~fields ~name;
    "string";
  ] |> String.concat "\n"


  let enum_tpl name fields =
    if List.length fields = 1 then (
      Logs.debug (fun m -> m "Ignoring %s, only one field\n" name); ""
    )
    else
      let body = [
        typet_tpl ~fields;
        enc_tpl ~fields ~name;
      ] |> String.concat "\n"
      in struct_tpl ~name ~body

  let get_enums tbl schema_js path =
    Logs.debug (fun m -> m "got string under '%s'\n"  (Q.json_pointer_of_path ~wildcards:true path));
    let aux ~path ~field =
      try
        let enum_path = path @ [`Field field] in
        match Q.query enum_path schema_js with
        | `A names ->
          let names =
            List.map Ezjsonm.decode_string_exn names
          in
          let module_name = match List.rev path with
          | `Field "command" :: _ -> "Command_enum" (* Request command types *)
          | `Field "event" :: _ -> "Event_enum" (* Event types *)
          (* | `Field "message" :: _ ->
           *   if field = "_enum" && List.length names = 1 && List.hd names = "cancelled" then
           *     "Message_enum" (\* Response message types - only one currently *\)
           *   else
           *     make_module_name ~path:enum_path *)
          | _ -> make_module_name ~path:enum_path
          in
          let xs = StrHashtbl.find_opt tbl module_name |> Option.value ~default:[] in
          StrHashtbl.replace tbl module_name (names @ xs);
        | _ -> ()
      with _ -> ()
    in
    aux ~path ~field:"_enum";
    aux ~path ~field:"enum"

  let render schema_js io =
    let t = WalkSchema.make ~strings:get_enums in
    WalkSchema.process t schema_js
    |> StrHashtbl.to_seq
    |> Seq.iter (fun (name, fields) -> Printf.fprintf io "%s\n" (enum_tpl name fields))

end


let () =
  Logs.set_reporter (Logs.format_reporter ());
  Logs.set_level (Some Logs.Info)
