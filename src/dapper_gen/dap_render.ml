module Sp = Dap_specs
module CommandHelper = Dap_dfs.CommandHelper
module EventHelper = Dap_dfs.EventHelper
module Dfs = Dap_dfs.Dfs


module type RenderT = sig
  type spec
  type t
  val of_spec : spec -> t
  val render : t -> name:string -> string*string

end

module RenderEnum : (RenderT with type spec := Sp.Enum_spec.t) = struct

  type t = Sp.Enum_spec.t

  let of_spec spec = spec

  let render (t:t) ~name =
    let t_str =
      let lns = t.enums |> List.map (fun (e:Sp.Enum_spec.enum_val) -> e.safe_name) |> String.concat " | " in
      let lns = if t.suggested then lns ^ " | Other of string" else lns in
      Printf.sprintf "type t = %s " lns
    in

    let enc_t_s =
      let lns = t.enums |> List.map (fun (e:Sp.Enum_spec.enum_val) -> Printf.sprintf "%s -> \"%s\"" e.safe_name e.dirty_name) in
      let lns = if t.suggested then lns @ ["Other s -> s"] else lns in
      String.concat " | " lns
    in
    let enc_s_t =
      let lns = t.enums |> List.map (fun (e:Sp.Enum_spec.enum_val) -> Printf.sprintf "\"%s\" -> %s" e.dirty_name e.safe_name) in
      let lns = if t.suggested then lns @ ["_ as s -> Other s"] else lns @ [Printf.sprintf "_ -> failwith \"%s\"" name] in
      String.concat " | " lns
    in
    let enc_str = Printf.sprintf
        "let enc = \n \
         let open Data_encoding in \n \
         conv \n \
         (function %s)\n \
         (function %s)\n \
         string" enc_t_s enc_s_t
    in
    Printf.sprintf "module %s = struct \n%s\n \n%s\n \nend\n" name t_str enc_str, ""

end


module RenderEnumWithPhantoms : (RenderT with type spec := Sp.Enum_spec.t) = struct

  type t = Sp.Enum_spec.t

  let of_spec spec = spec

  let render_ml (t:t) ~name =
    let t_str =
      let lns = t.enums |> List.map (fun (e:Sp.Enum_spec.enum_val) -> e.safe_name |> String.capitalize_ascii) |> String.concat " | " in
      Printf.sprintf "(* NOTE autogenerating %s, do not manually edit *)\ntype v = %s\n\ntype 'a t = v" name lns
    in


    let types_str =
      t.enums |> List.map (fun (e:Sp.Enum_spec.enum_val) ->
          let s = e.safe_name |> String.uncapitalize_ascii in
          Printf.sprintf "type %s" s
        ) |> String.concat "\n"
    in

    let ctors_str =
      t.enums |> List.map (fun (e:Sp.Enum_spec.enum_val) ->
          let l = e.safe_name |> String.uncapitalize_ascii in
          let u = l |> String.capitalize_ascii in
          Printf.sprintf "let %s : %s t = %s" l l u
        ) |> String.concat "\n"
    in

    let func_str reversed =
      let lns = t.enums |> List.map (fun (e:Sp.Enum_spec.enum_val) ->
          let u = e.safe_name |> String.capitalize_ascii in
          let d = e.dirty_name in
          if not reversed then
            Printf.sprintf "| (%s : _ t) -> \"%s\"" u d
          else
            Printf.sprintf "| \"%s\" -> (%s : _ t)" d u
        ) |> String.concat "\n"
      in
      let lns = if reversed then
          lns ^ (Printf.sprintf "| _ -> failwith \"%s\"\n" name)
        else
          lns
      in
      lns
    in


    let f_str =
      Printf.sprintf "let f : type a. a t -> string = function %s" @@ func_str false
    in

    let g_str =
      Printf.sprintf "let g : type a. string -> a t = function %s" @@ func_str true
    in

    let enc_str =
      "let enc = Data_encoding.conv f g Data_encoding.string\n\n"
    in
    [t_str; types_str; ctors_str; f_str; g_str; enc_str; ] |> String.concat "\n\n"

  let render_mli (t:t) ~name =
    let types_str =
      let lns = t.enums |> List.map (fun (e:Sp.Enum_spec.enum_val) ->
          let s = e.safe_name |> String.uncapitalize_ascii in
          Printf.sprintf "type %s" s
        ) |> String.concat "\n"
      in
      Printf.sprintf "(* NOTE autogenerating %s, do not manually edit *)\ntype 'a t\n%s" name lns
    in

    let ctors_str =
      t.enums |> List.map (fun (e:Sp.Enum_spec.enum_val) ->
          let l = e.safe_name |> String.uncapitalize_ascii in
          Printf.sprintf "val %s : %s t" l l
        ) |> String.concat "\n"
    in

    let enc_str =
      "val enc : 'a t Data_encoding.t"
    in
    [types_str; ctors_str; enc_str; ] |> String.concat "\n\n"


  let render (t:t) ~name =
    let ml = render_ml t ~name in
    let mli = render_mli t ~name in
    ml, mli

end


module RenderObjectField = struct

  type t = Sp.Field_spec.t

  let typ_str ~required ~seq type_ =
    let s = Printf.sprintf (if seq then "%s list" else "%s") type_ in
    Printf.sprintf (if required then "%s" else "%s option") s

  let req_enc_str ~required = if required then "req" else "opt"
  let seq_enc_str ~seq name = if seq then Printf.sprintf "(list %s)" name else name

  let render_t = function
    (* for the type t decl
       NOTE if it is cyclic field then hardcode type name to 't' *)
    | Sp.Field_spec.{ safe_name; type_; required; cyclic; seq; _ } when not cyclic ->
      Printf.sprintf "%s: %s;" safe_name @@ typ_str ~required ~seq type_
    | Sp.Field_spec.{ safe_name; required; seq; _ } ->
      Printf.sprintf "%s: %s;" safe_name @@ typ_str ~required ~seq "t"

  let render_enc = function
    (* for the encoding function
       NOTE if it is a cyclic field then hardcode the enc name to 'e' *)
    | Sp.Field_spec.{ dirty_name; enc_; required; cyclic; seq; _ } when not cyclic ->
      Printf.sprintf "(%s \"%s\" %s)" (req_enc_str ~required) dirty_name (seq_enc_str ~seq enc_)
    | Sp.Field_spec.{ dirty_name; required; seq; _ } ->
      Printf.sprintf "(%s \"%s\" %s)" (req_enc_str ~required) dirty_name (seq_enc_str ~seq "e")

  let render_arg (t:t) =
    (* for the make function *)
    Printf.sprintf "%s%s" (if t.required then "~" else "?") t.safe_name

end


module RenderObject : (RenderT with type spec := Sp.Obj_spec.t) = struct

  type t = Sp.Obj_spec.t

  let of_spec = function
    | Sp.Obj_spec.{fields; _} as spec when 10 >= List.length fields ->
      spec
    | _ -> failwith "Use RenderLargeObject"

  let render (t:t) ~name =
    let t_str =
      let lns = t.fields |> List.map RenderObjectField.render_t |> String.concat "\n" in
      Printf.sprintf "type t = { %s }" lns
    in
    let enc_obj_str =
      let lns = t.fields |> List.map RenderObjectField.render_enc |> String.concat "\n" in
      Printf.sprintf "(obj%d\n%s)\n" (List.length t.fields) lns
    in
    let rec_str =
      let ss = t.fields |> List.map (fun (f:Sp.Field_spec.t) -> f.safe_name) |> String.concat "; " in
      Printf.sprintf "{%s}" ss
    in
    let tup_str =
      let ss = t.fields |> List.map (fun (f:Sp.Field_spec.t) -> f.safe_name) |> String.concat ", " in
      if List.length t.fields = 1 then Printf.sprintf "%s" ss else Printf.sprintf "(%s)" ss
    in
    let args_str =
      t.fields |> List.map RenderObjectField.render_arg |> String.concat " "
    in

    let enc_str =
      let fmt = if t.is_cyclic then
          Printf.sprintf
            "let enc = \n \
             let open Data_encoding in \n \
             mu \"%s.t\" \n \
             ( fun e -> \n \
             conv \n \
             (fun %s -> %s)\n \
             (fun %s -> %s)\n \
             %s)"
          else Printf.sprintf
            "let enc = \n \
             let open Data_encoding in \n \
             (* %s.t *)
         conv \n \
             (fun %s -> %s)\n \
             (fun %s -> %s)\n \
             %s" in
      fmt name rec_str tup_str tup_str rec_str enc_obj_str
    in
    let make_str = Printf.sprintf
        "let make %s () = \n%s" args_str rec_str
    in
    Printf.sprintf "module %s = struct \n%s\n \n%s\n \n%s\n \nend\n" name t_str enc_str make_str, ""

end


module RenderLargeObject : (RenderT with type spec := Sp.Obj_spec.t) = struct
  (* NOTE objects with more than 10 fields need to be built with merge_obj *)

  type t = {
    spec: Sp.Obj_spec.t;
    ngroups: int;
    nleftover: int;
  }

  let of_spec = function
    | Sp.Obj_spec.{fields; is_cyclic; _} as spec when 10 < List.length fields ->
      if is_cyclic then failwith "TODO cyclic big objects" else (
        let n = List.length fields in
        let ngroups = n / 10 in
        let nleftover = n mod 10 in
        {spec; ngroups; nleftover}
      )
    | _ -> failwith "Use RenderObject"

  let render {spec; ngroups; nleftover} ~name =
    let fields_arr = Array.of_list spec.fields in
    let n = Array.length fields_arr in
    assert (nleftover + (ngroups * 10) = n);

    let modstrs =
      let ss =
        List.init ngroups succ
        |> List.fold_left (fun acc i ->
            let i = 10 * (i-1) in
            let fields = Array.sub fields_arr i 10 |> Array.to_list in
            let dirty_name = Printf.sprintf "%s_%d" spec.safe_name i in
            let spec = Sp.Obj_spec.of_path ~dirty_name ~path:[] ~fields () in
            let name = Printf.sprintf "%s_%d" name i in
            let modstr, _ = RenderObject.(of_spec spec |> render ~name) in
            (name, modstr, fields) :: acc
          ) [] in

      (* leftover fields *)
      let i = 10 * ngroups in
      let fields = Array.sub fields_arr i nleftover |> Array.to_list in
      let dirty_name = Printf.sprintf "%s_%d" spec.safe_name i in
      let spec = Sp.Obj_spec.of_path ~dirty_name ~path:[] ~fields () in
      let name = Printf.sprintf "%s_%d" name i in
      let modstr, _ = RenderObject.(of_spec spec |> render ~name) in
      List.rev @@ (name, modstr, fields) :: ss
    in
    let internal_mods = modstrs |> List.map (fun (_, modstr, _) -> modstr) |> String.concat "\n\n" in

    let rec aux_brkts ~sep = function
      | x :: [y] -> Printf.sprintf "(%s %s %s)" x sep y
      | ln :: rest ->
        let lns = aux_brkts ~sep rest in
        Printf.sprintf "(%s %s %s)" ln sep lns
      | [] -> ""
    in
    let t_str =
      Printf.sprintf "type t = %s" @@ aux_brkts ~sep:"*" (modstrs |> List.map (fun (nm, _, _) -> nm^".t"))
    in
    let enc_str =
      let rec aux = function
        | x :: [y] -> Printf.sprintf "merge_objs %s %s" x y
        | ln :: rest ->
          let lns = aux rest in
          Printf.sprintf "merge_objs %s @@ %s" ln lns
        | [] -> ""
      in
      let ln = aux (modstrs |> List.map (fun (nm, _, _) -> nm^".enc")) in
      Printf.sprintf
          "let enc = \n \
           let open Data_encoding in \n \
           %s" ln
    in
    let args_str =
      spec.fields |> List.map RenderObjectField.render_arg |> String.concat " "
    in
    let rec_str =
      let ss =
        modstrs |> List.mapi (fun i (nm, _, fields) ->
            let fields = fields |> List.map RenderObjectField.render_arg |> String.concat " " in
            Printf.sprintf "let t%d = %s.make %s () in" i nm fields
          ) |> String.concat "\n\n" in
      let tt = aux_brkts ~sep:"," (modstrs |> List.mapi (fun i _ -> Printf.sprintf "t%d" i)) in
      Printf.sprintf "%s\n\n%s" ss tt
    in
    let make_str = Printf.sprintf
        "let make %s () = \n%s" args_str rec_str
    in
    Printf.sprintf "module %s = struct \n%s\n \n%s\n \n%s\n \n%s\n \nend\n" name internal_mods t_str enc_str make_str, ""

end

module RenderEmptyObject : (RenderT with type spec := unit) = struct

  type t = unit

  let of_spec () = ()

  let render _t ~name =
    let t_str =
      "type t = unit"
    in

    let enc_str =
      "let enc = Data_encoding.empty"
    in
    let make_str =
        "let make () = () "
    in
    Printf.sprintf "module %s = struct \n%s\n \n%s\n \n%s\n \nend\n" name t_str enc_str make_str, ""

end


module RenderRequest : (RenderT with type spec := Sp.Obj_spec.t) = struct

  type t = Sp.Obj_spec.t

  let of_spec spec = spec

  let render (t:t) ~name =
    let command = CommandHelper.struct_decl_str name ~on:"Request" in
    match t.fields |> List.find_opt (fun Sp.Field_spec.{safe_name; _} -> safe_name = "arguments") with
    | Some args when args.required ->
      Printf.sprintf
        "module %sMessage = MakeRequest (%s)"
        name
        command
      ,
      Printf.sprintf
        "| %s of %s.t %sMessage.t"
        name
        args.module_name
        name

    | Some args ->
      Printf.sprintf
        "module %sMessage = MakeRequest_optionalArgs (%s)"
        name
        command
      ,
      Printf.sprintf
        "| %s of %s.t %sMessage.t"
        name
        args.module_name
        name

    | None ->
      Printf.sprintf
        "module %sMessage = MakeRequest_optionalArgs (%s)"
        name
        command
      ,
      Printf.sprintf
        "| %s of %s.t %sMessage.t"
        name
        Dap_t.EmptyObject.module_name
        name

end

module RenderResponse : (RenderT with type spec := Sp.Obj_spec.t) = struct

  type t = Sp.Obj_spec.t

  let of_spec spec = spec

  let render (t:t) ~name =
    let command = CommandHelper.struct_decl_str name ~on:"Response" in
    match t.fields |> List.find_opt (fun Sp.Field_spec.{safe_name; _} -> safe_name = "body") with
    | Some body when body.required ->
      Printf.sprintf
        "module %sMessage = MakeResponse (%s)"
        name
        command
      ,
      Printf.sprintf
        "| %s of %s.t %sMessage.t"
        name
        body.module_name
        name

    | Some body ->
      Printf.sprintf
        "module %sMessage = MakeResponse_optionalBody (%s)"
        name
        command
      ,
      Printf.sprintf
        "| %s of %s.t %sMessage.t"
        name
        body.module_name
        name

    | None ->
      Printf.sprintf
        "module %sMessage = MakeResponse_optionalBody (%s)"
        name
        command
      ,
      Printf.sprintf
        "| %s of %s.t %sMessage.t"
        name
        Dap_t.EmptyObject.module_name
        name

end

module RenderEvent : (RenderT with type spec := Sp.Obj_spec.t) = struct

  type t = Sp.Obj_spec.t

  let of_spec spec = spec

  let render (t:t) ~name =
    let event = EventHelper.struct_decl_str name in
    match t.fields |> List.find_opt (fun Sp.Field_spec.{safe_name; _} -> safe_name = "body") with
    | Some body when body.required ->
      Printf.sprintf
        "module %sMessage = MakeEvent (%s)"
        name
        event
      ,
      Printf.sprintf
        "| %s of %s.t %sMessage.t"
        name
        body.module_name
        name
    | Some body ->
      Printf.sprintf
        "module %sMessage = MakeEvent_optionalBody (%s)"
        name
        event
      ,
      Printf.sprintf
        "| %s of %s.t %sMessage.t"
        name
        body.module_name
        name
    | None ->
      Printf.sprintf
        "module %sMessage = MakeEvent_optionalBody (%s)"
        name
        event
      ,
      Printf.sprintf
        "| %s of %s.t %sMessage.t"
        name
        Dap_t.EmptyObject.module_name
        name

end
type ftype = | ML | MLI

type what =
  | Events of ftype | Commands of ftype | Messages

let render (dfs:Dfs.t) = function
  | Messages ->
    let modstrs = ref [] in
    let reqstrs = ref [] in
    let respstrs = ref [] in
    let eventstrs = ref [] in
    let _ =
      Dfs.ordering dfs
      |> List.iter (fun name ->
          match (Hashtbl.find_opt dfs.finished name) with
          | Some (Sp.Request o) ->
            let modstr, tystr = RenderRequest.(of_spec o |> render ~name) in
            modstrs := modstr :: !modstrs; reqstrs := tystr :: !reqstrs
          | Some (Sp.Response o) ->
            let modstr, tystr = RenderResponse.(of_spec o |> render ~name) in
            modstrs := modstr :: !modstrs; respstrs := tystr :: !respstrs
          | Some (Sp.Event o) ->
            let modstr, tystr = RenderEvent.(of_spec o |> render ~name) in
            modstrs := modstr :: !modstrs; eventstrs := tystr :: !eventstrs
          | Some (Sp.Object o) when Sp.Obj_spec.is_big o ->
            let modstr, _other = RenderLargeObject.(of_spec o |> render ~name) in
            modstrs := modstr :: !modstrs
          | Some (Sp.Object o) when Sp.Obj_spec.is_empty o ->
            let modstr, _other = RenderEmptyObject.(of_spec () |> render ~name) in
            modstrs := modstr :: !modstrs
          | Some (Sp.Object o) ->
            let modstr, _other = RenderObject.(of_spec o |> render ~name) in
            modstrs := modstr :: !modstrs
          | Some (Sp.Enum e) ->
            let modstr, _other = RenderEnum.(of_spec e |> render ~name) in
            modstrs := modstr :: !modstrs
          | Some _ -> assert false
          | None -> Logs.warn (fun m -> m "couldn't find '%s'" name)
        )
    in
    let smods = String.concat "\n\n" (!modstrs |> List.rev) in
    let sreqs = String.concat "\n" (!reqstrs |> List.rev) in
    let sresps = String.concat "\n" (!respstrs |> List.rev) in
    let sevents = String.concat "\n" (!eventstrs |> List.rev) in
    Printf.sprintf
      "(* NOTE this file was autogenerated - do not modify by hand *)\n\nopen Dap_t\n\n%s\n\ntype request = \n%s\n\ntype response = \n%s\n\ntype event = \n%s\n\n"
      smods sreqs sresps sevents
  | Commands ML ->
    let ml, _ = RenderEnumWithPhantoms.(of_spec dfs.command_enum |> render ~name:CommandHelper.module_name) in
    ml
  | Commands MLI ->
    let _, mli = RenderEnumWithPhantoms.(of_spec dfs.command_enum |> render ~name:CommandHelper.module_name) in
    mli
  | Events ML ->
    let ml, _ = RenderEnumWithPhantoms.(of_spec dfs.event_enum |> render ~name:EventHelper.module_name) in
    ml
  | Events MLI ->
    let _, mli = RenderEnumWithPhantoms.(of_spec dfs.event_enum |> render ~name:EventHelper.module_name) in
    mli
