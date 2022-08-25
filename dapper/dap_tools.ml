module L = Dap_autogen.LeafNodes
module D = Dap_autogen.Dfs

module Enum_spec = Dap_autogen.Enum_spec
module Prop_spec = Dap_autogen.Prop_spec
module ModuleName = Dap_autogen.ModuleName


module RenderEnums = struct

  (* TODO there is an ErrorResponse but no Command.Error *)

  type t = {
    specs:L.enum_specs;
    is_suggestion: bool;
  }

  let struct_tpl (t:t) ~body =
    let specs = t.specs in
    let module_str = specs.field_name.safe_name
    in
    Printf.sprintf "\n(* WARN autogenerated - do not modify by hand *)\n\nmodule %s = struct\n%s\nend" module_str body

  let typet_tpl (t:t) =
    let s = t.specs.enums
          |> List.map (fun (e:Enum_spec.t) -> e.safe_name)
          |> String.concat " | "
    in
    let s = if t.is_suggestion then
        s ^ " | Other of string "
      else
        s
    in
    Printf.sprintf "type t = | %s" s

  let enc_s_t_tpl (t:t) =
    let specs = t.specs in
    let s = specs.enums
      |> List.map (fun (el:Enum_spec.t) -> Printf.sprintf "\"%s\" -> %s" el.field_name el.safe_name)
      |> String.concat " | "
    in
    if t.is_suggestion then
      Printf.sprintf "(function | %s | _ as s -> Other s)" s
    else
      Printf.sprintf "(function | %s | _ -> failwith \"Unknown %s\")" s specs.field_name.safe_name

  let enc_t_s_tpl (t:t) =
    let s = t.specs.enums
            |> List.map (fun (el:Enum_spec.t) -> Printf.sprintf "%s -> \"%s\"" el.safe_name el.field_name)
            |> String.concat " | "
    in
    if t.is_suggestion then
      Printf.sprintf "(function | %s | Other s -> s)" s
    else
      Printf.sprintf "(function | %s)" s

  let enc_tpl (t:t) = [
    "let enc = "; "let open Data_encoding in";
    "conv";
    enc_t_s_tpl t;
    enc_s_t_tpl t;
    "string";
  ] |> String.concat "\n"


  let render (t:t) =
    let body = [
      typet_tpl t;
      enc_tpl t;
    ] |> String.concat "\n"
    in
    struct_tpl t ~body

  let of_enum ~enum () =
    match enum with
    | `Enum specs -> {specs; is_suggestion=false}
    | `EnumSuggestions specs -> {specs; is_suggestion=true}
    | _ -> failwith "not an enum"


end


module RenderObjects = struct

  type t = {
    specs: L.object_specs;
    tbl: L.t D.Data.t;
  }

  let struct_tpl (t:t) ~body =
    let specs = t.specs in
    Printf.sprintf "\n(* WARN autogenerated - do not modify by hand *)\n\nmodule %s = struct\n%s\nend" specs.field_name.safe_name body

  let typet_tpl (t:t) =
    let s = t.specs.fields
          |> List.map (fun (p:Prop_spec.t) -> Printf.sprintf "%s: %s%s;" p.safe_name p.field_t (if not p.required then " option" else ""))
          |> String.concat "\n"
    in
    Printf.sprintf "type t = { %s }" s

  let enc_t_s_tpl (t:t) =
    let specs = t.specs in
    let srec = specs.fields
      |> List.map (fun (el:Prop_spec.t) -> el.safe_name)
      |> String.concat "; "
    in
    let stup = specs.fields
      |> List.map (fun (el:Prop_spec.t) -> el.safe_name)
      |> String.concat ", "
    in
    Printf.sprintf "(fun {%s} -> (%s))" srec stup

  let enc_s_t_tpl (t:t) =
    let specs = t.specs in
    let srec = specs.fields
      |> List.map (fun (el:Prop_spec.t) -> el.safe_name)
      |> String.concat "; "
    in
    let stup = specs.fields
      |> List.map (fun (el:Prop_spec.t) -> el.safe_name)
      |> String.concat ", "
    in
    Printf.sprintf "(fun (%s) -> {%s})" stup srec

  let enc_obj (t:t) =
    let n = List.length t.specs.fields in
    let sobj = Printf.sprintf "obj%d" n in
    let sfrags = t.specs.fields
                 |> List.map (fun (p:Prop_spec.t) -> Printf.sprintf "(%s \"%s\" %s)" (if p.required then "req" else "opt") p.field_name p.field_enc)
                   |> String.concat "\n"
    in
    Printf.sprintf "(%s\n%s\n)" sobj sfrags

  let enc_tpl (t:t) = [
    "let enc = "; "let open Data_encoding in";
    "conv";
    enc_t_s_tpl t;
    enc_s_t_tpl t;
    enc_obj t;
  ] |> String.concat "\n"

  let make_tpl (t:t) =
    let args =
      t.specs.fields
      |> List.map (fun (p:Prop_spec.t) -> Printf.sprintf "%s%s" (if p.required then "~" else "?") p.safe_name)
      |> String.concat " "
    in
    let rec_fields =
      t.specs.fields
      |> List.map (fun (p:Prop_spec.t) -> Printf.sprintf "%s" p.safe_name)
      |> String.concat "; "
    in
    Printf.sprintf "let make %s () = \n{%s}" args rec_fields

  let render (t:t) =
    let body = [
      typet_tpl t;
      enc_tpl t;
      make_tpl t;
    ] |> String.concat "\n"
    in
    struct_tpl t ~body

  let get_field_enc (prop_spec:Prop_spec.t) ~(tbl:L.t D.Data.t) =
    let field_type = prop_spec.field_type in
    try
      match D.Data.find tbl (ModuleName.to_js_ptr field_type) with
      | `Json -> "string" (* TODO this is param by an 'a "Json_encoding.any_ezjson_value" *)
      | `Field f -> f.encoder
      | `Enum e -> (ModuleName.to_enc e.field_name)
      | `EnumSuggestions e -> (ModuleName.to_enc e.field_name)
      | `EmptyObject _o -> Printf.sprintf "%s.enc" Dapper__Dap_autogen._EMPTY_OBJECT
      | `Object o -> (ModuleName.to_enc o.field_name)
      | `Ref (e, _is_cyclic) -> (ModuleName.to_enc e.field_type) (* TODO is cyclic *)
      | _ -> "string" (* TODO *)
    with _ -> "string"


  let get_field_t (prop_spec:Prop_spec.t) ~(tbl:L.t D.Data.t) =
    let field_type = prop_spec.field_type in
    try
      match D.Data.find tbl (ModuleName.to_js_ptr field_type) with
      | `Json -> "string" (* TODO see above "Json_repr.ezjsonm" *)
      | `Field f -> f.encoder
      | `Enum e -> (ModuleName.to_type_t e.field_name)
      | `EnumSuggestions e -> (ModuleName.to_type_t e.field_name)
      | `EmptyObject _o -> Printf.sprintf "%s.t" Dapper__Dap_autogen._EMPTY_OBJECT
      | `Object o -> (ModuleName.to_type_t o.field_name)
      | `Ref (e, _is_cyclic) -> (ModuleName.to_type_t e.field_type)
      | _ -> "string" (* TODO *)
    with _ -> "string"

  let of_object ~obj ~tbl () =
    let specs : L.object_specs = match obj with
      | `Object specs_in -> specs_in
      | _ -> failwith "not an object"
    in
    if List.length specs.fields > 10 then
      (Logs.warn (fun m -> m "Object %s has more than 10 fields" specs.field_name.safe_name); failwith "error")
    else (


      let fields = specs.fields
                   |> List.map (fun (prop_spec:Prop_spec.t) ->
                       let field_enc = get_field_enc prop_spec ~tbl in
                       let field_t = get_field_t prop_spec ~tbl in
                       {prop_spec with field_enc; field_t})
      in
      let specs = {specs with fields} in
      {specs; tbl}
    )

end


let () =
  Logs.set_reporter (Logs.format_reporter ());
  Logs.set_level (Some Logs.Info);
  let schema_js = Ezjsonm.from_channel @@ open_in "../schema/debugAdapterProtocol-1.56.X.json" in
  let t = D.make ~schema_js in
  let io = open_out "test1.ml" in
  Printf.fprintf io "open Dap_t\n\n";
  Logs.debug (fun m -> m "rendering enums start");
  D.get_enums t
  |> List.iter (fun enum -> let s = RenderEnums.(of_enum ~enum () |> render) in Printf.fprintf io "%s\n" s);
  Logs.debug (fun m -> m "rendering enums end");
  (* close_out io;
   *
   * let io = open_out "test1.ml" in *)
  Logs.debug (fun m -> m "getting all elements");
  let tbl = match D._get t ~what:`Elements with | Leaves tbl -> tbl | _ -> failwith "error" in
  Logs.debug (fun m -> m "rendering objects start");
  D.get_objects t
  |> List.iter (fun obj -> try let s = RenderObjects.(of_object ~obj ~tbl () |> render) in Printf.fprintf io "%s\n" s with _ -> ());
  Logs.debug (fun m -> m "rendering objects start");
  close_out io
