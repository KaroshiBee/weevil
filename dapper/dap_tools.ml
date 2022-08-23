module L = Dap_autogen.LeafNodes
module D = Dap_autogen.Dfs
module Enum_spec = Dap_autogen.Enum_spec


module RenderEnums = struct

  (* TODO there is an ErrorResponse but no Command.Error *)

  type t = {
    specs:L.enum_specs;
    is_suggestion: bool;
  }

  let struct_tpl (t:t) ~body =
    let specs = t.specs in
    Printf.sprintf "\n(* WARN autogenerated - do not modify by hand *)\n\nmodule %s = struct\n%s\nend" specs.field_name.safe_name body

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


let () =
  Logs.set_reporter (Logs.format_reporter ());
  Logs.set_level (Some Logs.Info);
  let schema_js = Ezjsonm.from_channel @@ open_in "../schema/debugAdapterProtocol-1.56.X.json" in
  let t = D.make ~schema_js in
  let io = open_out "test1.ml" in
  D.get_enums t
  |> List.iter (fun enum -> let s = RenderEnums.(of_enum ~enum () |> render) in Printf.fprintf io "%s\n" s);
  D.get_enum_suggestions t
  |> List.iter (fun enum -> let s = RenderEnums.(of_enum ~enum () |> render) in Printf.fprintf io "%s\n" s);
  close_out io
