module Q = Json_query

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
      | `Field "command" :: _ -> "Command" (* Request/Response command types *)
      | `Field "event" :: _ -> "Event" (* Event types *)
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
    | `Command command_value -> Printf.sprintf "struct let command=Command.%s end" command_value
    | `Event event_value -> Printf.sprintf "struct let event=Event.%s end" event_value

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
      args_name: module_name;
    }

    let render t =
      Printf.sprintf
        "module %s = MakeRequest (%s) (%s)"
        (ModuleName.to_module_name t.module_name)
        (ModuleName.to_functor_arg (`Command t.command_value))
        (ModuleName.to_module_name t.args_name)
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
          "EmptyObject"

  end

  module EventSpec = struct
    type t = {
      module_name: module_name;
      event_value: string;
      body_name: module_name;
    }

    let render t =
      Printf.sprintf
        "module %s = MakeEvent (%s) (%s)"
        (ModuleName.to_module_name t.module_name)
        (ModuleName.to_functor_arg (`Event t.event_value))
        (ModuleName.to_module_name t.body_name)
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
