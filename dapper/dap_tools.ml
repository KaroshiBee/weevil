open Json_schema
module Q = Json_query


module StrHashtbl = Hashtbl.Make(struct type t = string let equal = String.equal let hash = Hashtbl.hash end)

module ModuleName = struct

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

  let make_module_name ~path =
    match List.rev path with
    | `Field "command" :: _ -> "Command" (* Request command types *)
    | `Field "event" :: _ -> "Event" (* Event types *)
    (* | `Field "message" :: _ ->
             *   if field = "_enum" && List.length names = 1 && List.hd names = "cancelled" then
             *     "Message_enum" (\* Response message types - only one currently *\)
             *   else
             *     make_module_name ~path:enum_path *)
    | _ -> _make_module_name ~path

end


(*
 * the _enum fields arent picked up by Json_schema module
 * and so using Json_schema.to_json wont work.
 * Have to use Ezjsonm.from_channel to read the raw json and then query that.
 * It is for this reason that also have to manually add some stuff to a path as
 * recursion progresses (e.g. "properties" or "allOf" > index 1)
*)


module WalkSchema = struct

  let _nothing _tbl _string_js _path _stuff = ()
  let _nothing2 _tbl _string_js _path _newpath _stuff = ()

  type prop_ty = EmptyObj of object_specs | Obj of object_specs

  type ('a, 'b, 'c) t = {
    strings: 'a StrHashtbl.t -> Json_repr.ezjsonm -> Json_query.path -> Json_schema.element_kind -> unit;
    def_ref: 'b StrHashtbl.t -> Json_repr.ezjsonm -> Json_query.path -> Json_schema.element_kind -> unit;
    props: 'c StrHashtbl.t -> Json_repr.ezjsonm -> Json_query.path -> Json_query.path -> Json_schema.element_kind -> unit;
  }

  let make ?(strings=_nothing) ?(def_ref=_nothing) ?(props=_nothing2) () =
    {strings; def_ref; props}


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
        if List.length properties = 0 then
          let obj_specs = EmptyObj {properties=[]; pattern_properties; additional_properties; min_properties; max_properties; schema_dependencies; property_dependencies} in
          process_property t ~tbl ~schema_js ~path obj_specs
        else
          properties
          |> List.iter (fun (pname, ty, required, extra) ->
              let obj_specs = Obj {properties=[(pname, ty, required, extra)]; pattern_properties; additional_properties; min_properties; max_properties; schema_dependencies; property_dependencies} in
              process_property t ~tbl ~schema_js ~path obj_specs
            )

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
            Logs.debug (fun m -> m "process combination allOf with %d elements under '%s'\n"  (List.length elements) (Q.json_pointer_of_path ~wildcards:true path));
            elements
              |> List.iteri (fun i el ->
                let new_path = path @ [`Field "allOf"; `Index i] in
                process_element t ~tbl ~schema_js ~path:new_path el
              )
          )
        | One_of -> (
            Logs.debug (fun m -> m "process combination oneOf with %d elements under '%s'\n"  (List.length elements) (Q.json_pointer_of_path ~wildcards:true path));
            elements
              |> List.iteri (fun i el ->
                let new_path = path @ [`Field "oneOf"; `Index i] in
                process_element t ~tbl ~schema_js ~path:new_path el
              )
          )
        | Any_of | Not -> () (* failwith "TODO other combinators" *)
      )
    | Def_ref _ as x -> t.def_ref tbl schema_js path x
    | Id_ref _ -> () (* failwith "TODO Id_ref" *)
    | Ext_ref _ -> () (* failwith "TODO Ext_ref" *)
    | String _ as s -> t.strings tbl schema_js path s
    | Integer _ -> () (* failwith "TODO Integer" *)
    | Number _ -> () (* failwith "TODO Number" *)
    | Boolean -> () (* failwith "TODO Boolean" *)
    | Null -> () (* failwith "TODO Null" *)
    | Any -> () (* failwith "TODO Any" *)
    | Dummy -> () (* failwith "TODO Dummy" *)

  and process_property t ~tbl ~schema_js ~path = function
    (* NOTE can be empty object, so only recurse if len=1 *)
    | EmptyObj specs -> t.props tbl schema_js path path (Object specs)
    | Obj specs -> (
      let (pname, element, _, _) = List. hd specs.properties in
      Logs.debug (fun m -> m "process property '%s' under '%s'\n"  pname (Q.json_pointer_of_path ~wildcards:true path));
      let new_path = path @ [`Field "properties"; `Field pname ] in
      t.props tbl schema_js path new_path (Object specs);
      process_element t ~tbl ~schema_js ~path:new_path element
    )


  let process t schema_js =
    let names = function
      | `O fields -> fields |> List.map (fun (nm, _) -> nm)
      | _ -> []
    in
    let tbl = StrHashtbl.create 500 in
    let ns = Q.query [`Field "definitions"] schema_js |> names in
    Logs.debug (fun m -> m "\n\nprocessing '%d' names\n" @@ List.length ns);
    ns |> List.iter (fun nm ->
        let path = [`Field "definitions"; `Field nm]
        in process_dfn t ~tbl ~schema_js ~path
      );
    tbl

end


module Enums = struct

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

  let get_enums tbl schema_js path _stuff =
    Logs.debug (fun m -> m "got string under '%s'\n"  (Q.json_pointer_of_path ~wildcards:true path));
    let aux ~path ~field =
      try
        let enum_path = path @ [`Field field] in
        match Q.query enum_path schema_js with
        | `A names ->
          let names =
            List.map Ezjsonm.decode_string_exn names
          in
          let module_name = ModuleName.make_module_name ~path in
          let xs = StrHashtbl.find_opt tbl module_name |> Option.value ~default:[] in
          StrHashtbl.replace tbl module_name (names @ xs);
        | _ -> ()
      with _ -> ()
    in
    aux ~path ~field:"_enum";
    aux ~path ~field:"enum"

  let walk schema_js =
    let t = WalkSchema.make ~strings:get_enums () in
    WalkSchema.process t schema_js

  let render tbl io =
    tbl
      |> StrHashtbl.to_seq
      |> Seq.iter (fun (name, fields) -> Printf.fprintf io "%s\n" (enum_tpl name fields))

end

module CombDeps = struct

  type t = combinator option

  let to_string = function
    | Some All_of -> "allOf"
    | Some One_of -> "oneOf"
    | Some Any_of -> "anyOf"
    | Some Not -> "not"
    | None -> ""

  let of_string = function
    | "allOf" -> Some All_of
    | "oneOf" -> Some One_of
    | "anyOf" -> Some Any_of
    | "not" -> Some Not
    | _ -> None

  let of_path_tip ~path =
    match List.rev path with
    | `Index _ :: `Field s :: _ -> s |> of_string |> to_string
    | _ -> ""


end

module Dependencies = struct

  let def_ref tbl _schema_js path = function
    | Def_ref ref_path -> (
        Logs.info (fun m -> m "Dependencies - def_ref: path '%s', ref_path: '%s'" (Q.json_pointer_of_path path) (Q.json_pointer_of_path ref_path));
        let name = ModuleName.make_module_name ~path in
        let combstr = CombDeps.of_path_tip ~path in
        (* (\* need to strip off last field ie go up a level *\)
         * let name = match List.rev path with
         * | _ :: tl -> make_module_name ~path:(List.rev tl)
         * | _ -> make_module_name ~path
         * in *)
        let path_str = Q.json_pointer_of_path ref_path in
        (* add path_str to the entries under name *)
        let ps = StrHashtbl.find_opt tbl name |> Option.value ~default:[] in
        StrHashtbl.replace tbl name ((combstr, path_str) :: ps)
      )
    | _ -> Logs.warn (fun m -> m "Wasn't expecting non-def-ref element kind")

  let props tbl _ path new_path = function
    | Object _obj_specs -> (
        Logs.info (fun m -> m "Dependencies - props: path '%s', new_path: '%s'" (Q.json_pointer_of_path path) (Q.json_pointer_of_path new_path));
        let name = ModuleName.make_module_name ~path in
        let combstr = CombDeps.of_path_tip ~path in
        let path_str = Q.json_pointer_of_path new_path in
        let ps = StrHashtbl.find_opt tbl name |> Option.value ~default:[] in
        StrHashtbl.replace tbl name ((combstr, path_str) :: ps)
      )
    | _ -> ()

  let walk schema_js =
    let t = WalkSchema.make ~def_ref ~props () in
    WalkSchema.process t schema_js

  let render tbl io =
    tbl
    |> StrHashtbl.to_seq
    |> List.of_seq
    |> List.sort (fun (x, _) (y, _) -> String.compare x y)
    |> List.iter (fun (name, fields) ->
        Printf.fprintf io "%s:\n    %s\n"
          name
          (String.concat "\n    " (fields |> List.map (fun (comb, f) -> Printf.sprintf "(%s, %s)" comb f)))
      )

end


module Objects = struct
  (* TODO additional properties *)
  let props tbl _ path new_path = function
    | Object obj_specs -> (
        (* the key in the dict ie name of the type with the property fields *)
        let module_name = ModuleName.make_module_name ~path in
        let xs = StrHashtbl.find_opt tbl module_name |> Option.value ~default:[] in
        match obj_specs.properties with
        | [(name, _, required, _)] ->
          (* the name and path of a field, together with required flag *)
          let x = (name, Q.json_pointer_of_path new_path, required) in
          StrHashtbl.replace tbl module_name (x :: xs);
        | [] ->
          (* the name and path of a field, together with required flag *)
          let name = "NULL" in
          let required = false in
          let x = (name, Q.json_pointer_of_path new_path, required) in
          StrHashtbl.replace tbl module_name (x :: xs);
        | _ -> ()
      )
    | _ -> ()

  let walk schema_js =
    let t = WalkSchema.make ~props () in
    WalkSchema.process t schema_js

  let render tbl io =
    tbl
      |> StrHashtbl.to_seq
      |> Seq.iter (fun (module_name, stuff) ->
          stuff
          |> List.iter (fun (field_name, full_name, required) ->
              Printf.fprintf io "%s: (%s, %s, %b)\n" module_name field_name full_name required
            )
        )

end

let unweird_name nm =
  match (String.lowercase_ascii nm) with
  | "type" | "module" | "lazy" -> Printf.sprintf "%s_" nm
  | "null" -> "empty_"
  | _ -> nm

let get_type enums js js_ptr is_enc =
  Json_query.(
    let path = (path_of_json_pointer js_ptr) in
    let module_name = ModuleName.make_module_name ~path in
    match module_name with
    | "Request_type" | "Response_type" | "Event_type" ->
        Printf.sprintf "Dap_enum.ProtocolMessage_type.%s" (if is_enc then "enc" else "t")
    | "Response_message" -> "string"
    | _ -> (
        match StrHashtbl.find_opt enums module_name with
        | Some _ -> Printf.sprintf "%s.%s" module_name (if is_enc then "enc" else "t")
        | None -> (
            let pth = path @ [`Field "type"] in
            try
              match query pth js with
              | `String "integer" | `String "number" -> "int64"
              | `String "boolean" -> "bool"
              | `String "array" -> "string" (* TODO *)
              | `String "object" -> "string" (* TODO *)
              | `String s -> s
              | _ -> failwith (Printf.sprintf "unknown leaf type @ %s/type" js_ptr)
            with _ ->
              "string"
          )
      )
  )

let () =
  Logs.set_reporter (Logs.format_reporter ());
  Logs.set_level (Some Logs.Info);
  (* let js = Ezjsonm.from_channel @@ open_in "../schema/example2.json" in
   * let enums = Enums.walk js in
   * let _deps = Dependencies.walk js in
   * let objs = Objects.walk js in
   * let cc fields s =
   *   (fields |> List.map (fun (field_name, _, _) -> unweird_name field_name) |> String.concat s)
   * in
   *
   * let io = open_out "test2.ml" in
   * Printf.fprintf io "\n(\* WARN autogenerated - do not modify by hand *\)\nopen Dap_enum\n\n";
   * objs
   * |> StrHashtbl.iter (fun module_name fields ->
   *     (\* TODO Module class and ones with lots of fields *\)
   *     if String.equal "Module" module_name || List.length fields > 10 then () else (
   *       let n = List.length fields in
   *       Printf.fprintf io "\nmodule %s = struct\n" (unweird_name module_name);
   *       Printf.fprintf io "\ntype t = {\n";
   *       fields
   *       |> List.iter (fun (field_name, js_ptr, required) ->
   *           Printf.fprintf io "\n%s: %s%s;" (unweird_name field_name) (get_type enums js js_ptr false) (if required then "" else " option")
   *         );
   *       Printf.fprintf io "\n}\n";
   *
   *       Printf.fprintf io "\nlet enc = \nlet open Data_encoding in \nconv \n";
   *       Printf.fprintf io "(fun {%s} -> (%s)\n)" (cc fields "; ") (cc fields ", ");
   *       Printf.fprintf io "(fun (%s) -> {%s}\n)" (cc fields ", ") (cc fields "; ");
   *       Printf.fprintf io "(obj%n %s)" n (
   *         fields
   *         |> List.map (fun (field_name, js_ptr, required) ->
   *             Printf.sprintf "(%s \"%s\"%s)" (if required then "req" else "opt") field_name (get_type enums js js_ptr true)
   *           )
   *         |> String.concat "\n"
   *       );
   *
   *       Printf.fprintf io "\nlet make %s () = \n{%s}\n" (
   *         fields
   *         |> List.map (fun (field_name, _js_ptr, required) ->
   *             Printf.sprintf "%s%s" (if required then "~" else "?") (unweird_name field_name)
   *           )
   *         |> String.concat " "
   *       ) (
   *         fields
   *         |> List.map (fun (field_name, _js_ptr, _required) ->
   *             unweird_name field_name
   *           )
   *         |> String.concat "; "
   *       );
   *
   *
   *       Printf.fprintf io "\nend\n\n";
   *     )
   *   );
   * close_out io; *)
