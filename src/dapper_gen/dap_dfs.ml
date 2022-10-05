module Sp = Dap_specs
module Q = Json_query
module S = Json_schema

module CommandHelper = struct

  let _COMMAND = "Command"

  let strip_command name ~on =
    match Stringext.cut name ~on with
    | Some (enum_str, "") -> Printf.sprintf "struct let command=%s.%s end" _COMMAND enum_str
    | _ -> assert false

end

module EventHelper = struct

  let _EVENT = "Event"

  let strip_event name =
    match Stringext.cut name ~on:"Event" with
    | Some (enum_str, "") -> Printf.sprintf "struct let event=%s.%s end" _EVENT enum_str
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


module Dfs = struct

  type t = {
    schema_js: Json_repr.ezjsonm; [@printer Json_repr.pp (module Json_repr.Ezjsonm)]
    nodes: Sp.t list;
    ordering: string list;
    visited: Visited.t;
    finished: Finished.t;
  } [@@deriving show]

  let ordering t = t.ordering |> List.rev

  let _set_field_type t module_name type_ enc_ =
      match t.nodes with
      | Sp.Field field_spec :: nodes ->
        let field = Sp.Field_spec.{field_spec with module_name; type_; enc_} in
        let nodes = (Sp.Field field) :: nodes in
        {t with nodes}
      | _ -> t

  let _wrap_field_type t ~wrap =
      match t.nodes with
        | Sp.Field field_spec :: nodes ->
          let type_ = field_spec.type_ ^ " " ^ wrap in
          let enc_ = "(" ^ wrap ^ " " ^ field_spec.enc_ ^ ")" in
          let field = Sp.Field_spec.{field_spec with type_; enc_} in
          let nodes = (Sp.Field field) :: nodes in
          {t with nodes}
        | _ -> t

  let rec _make_module_name = function
    | `Field "definitions" :: rest
    | `Field "allOf" :: rest
    | `Field "anyOf" :: rest
    | `Field "oneOf" :: rest
    | `Field "not" :: rest
    | `Field "properties" :: rest
    | `Field "_enum" :: rest
    | `Index _ :: rest
    | `Next :: rest
    | `Star :: rest
      -> _make_module_name rest
    | `Field f :: rest -> (
      match _make_module_name rest with
      | "" -> f
      | s -> Printf.sprintf "%s_%s" f s
    )
    | [] -> ""



  let rec process_definition t ~path =
    (* this will be a *Request/*Response/*Event/Object top level definition
       will want to ignore base class ProtocolMessage, Request, Response, Event types
       as they are handled with the functors in Dap_t
       only want to recurse down into the defn if it is not already being/been visited
       and, if so, we want to recurse after setting the initial defn in elements map
       with the notion that some objects might get rewritten as enums
    *)
    let dfn = Q.json_pointer_of_path path in
    let module_name = _make_module_name path in

    Logs.debug (fun m -> m "process dfn start: '%s', '%s'" dfn module_name) ;
    let t =
      if Sp.is_special_definition ~path then (
        Logs.debug (fun m -> m "special case: '%s'" dfn) ;
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
          | Sp.Object _ as ospec :: nodes ->
            Logs.debug (fun m -> m "adding definition '%s' to finished pile with module name '%s'" dfn module_name) ;
            let ordering = module_name :: t.ordering in
            Hashtbl.add t.finished module_name ospec;
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

  and process_element t ~path el =
    (* can get proper enums here, still need to qry for _enums *)
    let dfn = Q.json_pointer_of_path path in
    let module_name = _make_module_name path in
    let dirty_name = Sp.dirty_name ~path |> Option.get in
    Logs.debug (fun m -> m "process element under '%s'" dfn);
    let enums = el.enum |> Option.value ~default:[] in
    let enums = enums
      |> List.map Json_repr.from_any
      |> List.map Ezjsonm.decode_string_exn
    in
    let n = List.length enums in
    (* it is an enum of some sort *)
    if n > 1 then (
      Logs.debug (fun m -> m "element under '%s' has enum [%s]" dfn (enums |> String.concat ", ")) ;
      let enum_spec = Sp.Enum_spec.of_path ~dirty_name ~path ~dirty_names:enums () in
      (* let module_name = *)
      (*   match *)
      (*     (Sp.Enum_spec.is_command enum_spec, Sp.Enum_spec.is_event enum_spec) *)
      (*   with *)
      (*   | (true, false) -> CommandHelper._COMMAND (\* TODO add as extra field to existing *\) *)
      (*   | (false, true) -> EventHelper._EVENT (\* TODO add as extra field to existing *\) *)
      (*   | (_, _) -> module_name *)
      (* in *)
      let ordering = module_name :: t.ordering in
      Hashtbl.add t.finished module_name (Sp.Enum enum_spec);
      {t with ordering}
    ) else (
      Logs.debug (fun m -> m "process element under '%s'" dfn) ;
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
         let module_name = _make_module_name new_path in
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

  and process_kind t ~path = function
    | Object o when (List.length o.properties) = 0 ->
        Logs.debug (fun m ->
            m
              "process object with 0 properties under '%s'"
              (Q.json_pointer_of_path ~wildcards:true path)) ;
        (* TODO things like Message.variables have additionalProperties *)
        _set_field_type t "" "Data_encoding.json" "json"

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
            (* need to filter out initial objects, these are objects that have been inline-defined *)
            let t =
              match t.nodes with
              | Sp.Object obj :: nodes ->
                let module_name = _make_module_name obj.path in
                Logs.debug (fun m -> m "adding inline object at '%s' to finished pile with module name '%s'" dfn module_name) ;
                let ordering = module_name :: t.ordering in
                Hashtbl.add t.finished module_name (Object obj);
                {t with nodes; ordering}
              | _ -> t
            in
            (* now pop off the n-top Fields of the node list and make the object spec *)
            let arr = Array.of_list t.nodes in
            Logs.debug (fun m -> m "n:%d, t:%s" n @@ show t);
            let fields = Array.sub arr 0 n |> Array.to_list |> List.map (fun f ->
                match f with | Sp.Field spec -> spec | s ->  failwith (Printf.sprintf "reading %d items, expected field, got %s" n (Sp.show s))) in
            match Array.sub arr n (Array.length arr - n) |> Array.to_list with
            | Sp.Object spec :: nodes' ->
              Logs.debug (fun m -> m "adding %d fields to object at '%s'" n @@ Q.json_pointer_of_path path) ;
              let nodes = Sp.(Object Obj_spec.{spec with fields} ) :: nodes' in
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
        _wrap_field_type t ~wrap:"list"
    | Monomorphic_array _ -> failwith "TODO marray"
    | Combine (All_of, [dref; el])
        when dref.kind = Def_ref [`Field "definitions"; `Field "Request"] ->
        Logs.debug (fun m -> m "process request under '%s'" (Q.json_pointer_of_path ~wildcards:true path)) ;
        let new_path = path @ [`Field "allOf"; `Index 1] in
        let t' = process_element t ~path:new_path el in
        Hashtbl.replace_seq t.visited (Hashtbl.to_seq t'.visited) ;
        t'
    | Combine (All_of, [dref; el])
        when dref.kind = Def_ref [`Field "definitions"; `Field "Event"] ->
        Logs.debug (fun m -> m "process event under '%s'" (Q.json_pointer_of_path ~wildcards:true path)) ;
        let new_path = path @ [`Field "allOf"; `Index 1] in
        let t' = process_element t ~path:new_path el in
        Hashtbl.replace_seq t.visited (Hashtbl.to_seq t'.visited) ;
        t'
    | Combine (All_of, [dref; el])
        when dref.kind = Def_ref [`Field "definitions"; `Field "Response"] ->
        Logs.debug (fun m -> m "process response under '%s'" (Q.json_pointer_of_path ~wildcards:true path)) ;
        let new_path = path @ [`Field "allOf"; `Index 1] in
        let t' = process_element t ~path:new_path el in
        Hashtbl.replace_seq t.visited (Hashtbl.to_seq t'.visited) ;
        t'

    | Combine (c, elements) -> (
        match c with
        | All_of ->
            Logs.debug (fun m ->
                m
                  "process combination allOf with %d elements under '%s'"
                  (List.length elements)
                  (Q.json_pointer_of_path ~wildcards:true path)) ;
            (* Logs.debug (fun m -> m "nodes before allOf: %s" @@ show t); *)
            let (_, t) =
              elements
              |> List.fold_left
                   (fun (i, t_acc) el ->
                     let new_path = path @ [`Field "allOf"; `Index i] in
                     let t' = process_element t_acc ~path:new_path el in
                     Hashtbl.replace_seq t_acc.visited (Hashtbl.to_seq t'.visited) ;
                     (i + 1, {t' with visited = t_acc.visited}))
                   (0, t)
            in
            (* Logs.debug (fun m -> m "nodes after allOf: %s" @@ show t); *)

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
            t
        | One_of -> failwith "TODO OneOf"
        | Any_of -> failwith "TODO AnyOf"
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
        let is_cyclic = false
        in

        (* Logs.debug (fun m -> m "nodes before def_ref '%s': %s" ref_path_str @@ show t); *)
        let t =
          if is_cyclic then t
          else (
            let t' = process_definition t ~path:ref_path in
            (* get typename from ref_path *)
            let ref_type_name = _make_module_name ref_path in
            _set_field_type t' ref_type_name (ref_type_name^".t") (ref_type_name^".enc")
          )
        in
        (* Logs.debug (fun m -> m "nodes after def_ref '%s': %s" ref_path_str @@ show t); *)
        t

    | Id_ref _ -> failwith "TODO Id_ref"
    | Ext_ref _ -> failwith "TODO Ext_ref"
    | String _ -> (
      (* NOTE if its an _enum set then parse as Enum because
         the _enum fields arent picked up by Json_schema module
         and so using Json_schema.to_json wont work.
         Have to use Ezjsonm.decode_string to read the raw json and then query that.
      *)
      let enum_path = path @ [`Field "_enum"] in
      let module_name = _make_module_name enum_path in
      let enum =
        try
          match Q.query enum_path t.schema_js with
          | `A names ->
            let dirty_names =
              List.map Ezjsonm.decode_string_exn names
            in
            Some (Sp.Enum_spec.of_path ~dirty_name:module_name ~path:enum_path ~dirty_names ())
          | _ -> None
        with _ -> None
      in
      match enum with
      | Some enum ->
        Logs.debug (fun m -> m "inlined Enum");
        Hashtbl.add t.finished module_name (Sp.Enum enum);
        let ordering = module_name :: t.ordering in
        _set_field_type {t with ordering} module_name (module_name^".t") (module_name^".enc")
      | None ->
        Logs.debug (fun m -> m "String");
        _set_field_type t "" "string" "string"
    )
    | Integer _ ->
      Logs.debug (fun m -> m "Int");
      _set_field_type t "" "int" "int31"
    | Number _ ->
      Logs.debug (fun m -> m "Number");
      _set_field_type t "" "int" "int31"
    | Boolean ->
      Logs.debug (fun m -> m "Bool");
      _set_field_type t "" "bool" "bool"
    | Null -> failwith "TODO Null"
    | Any -> failwith "TODO Any"
    | Dummy -> failwith "TODO Dummy"

  let make ~schema_js =
    let names = function
      | `O fields -> fields |> List.map (fun (nm, _) -> nm)
      | _ -> []
    in
    let empty = {schema_js; nodes = []; ordering = []; visited = Hashtbl.create 500; finished = Hashtbl.create 500} in
    (* NOTE know all /definitions/ are objects and are the only top-level things *)
    let ns = Q.query [`Field "definitions"] schema_js |> names in
    Logs.info (fun m -> m "\n\nprocessing '%d' names" @@ List.length ns) ;
    ns
    |> List.fold_left
         (fun t_acc nm ->
           let path = [`Field "definitions"; `Field nm] in
           let t = process_definition t_acc ~path in
           Hashtbl.replace_seq t_acc.visited (Hashtbl.to_seq t.visited) ;
           {t with visited = t_acc.visited})
         empty

  (* let process t = *)
  (*   let wksp : (string * Sp.Obj_spec.t) list ref = ref [] in *)
  (*   let rec aux = function *)
  (*     | Leaf (Value leaf_specs) :: Prop (Object prop_specs) :: tl -> *)
  (*       Logs.debug (fun m -> m "\n\nprocessing leaf '%s' and prop '%s'" (Sp.show_value leaf_specs) (Sp.Obj_spec.show prop_specs)) ; *)
  (*       wksp := (Sp.show_value leaf_specs, prop_specs) :: !wksp; *)
  (*       aux tl *)
  (*     | Leaf (Object leaf_specs) :: Prop (Object prop_specs) :: tl -> *)
  (*       Logs.debug (fun m -> m "\n\nprocessing leaf '%s' and prop '%s'" (Sp.Obj_spec.show leaf_specs) (Sp.Obj_spec.show prop_specs)) ; *)
  (*       wksp := ("any", prop_specs) :: !wksp; *)
  (*       aux tl *)
  (*     | Node (Sp.Object specs) :: tl -> *)
  (*       Logs.debug (fun m -> m "\n\nprocessing '%s'" @@ Sp.Obj_spec.show specs) ; *)
  (*       let fields = !wksp *)
  (*           |> List.map (fun (leaf, prop) -> *)
  (*                        let pth = List.rev Sp.Obj_spec.(prop.path) in *)
  (*                        let dirty_name = match List.hd pth with `Field nm -> nm | _ -> "unknown" in *)
  (*                        let safe_name = Sp._unweird_name dirty_name in *)
  (*                        let type_ = leaf in *)
  (*                        let enc_ = type_ in *)
  (*                        let required = true in *)
  (*                        Sp.Obj_spec.{dirty_name; safe_name; type_; enc_; required} *)
  (*                      ) in *)
  (*       wksp := []; *)
  (*       {specs with fields} :: aux tl *)

  (*     | _ -> [] *)
  (*   in *)
  (*   let nodes = aux t.nodes in *)
  (*   nodes *)


end


module type RenderT = sig
  type spec
  type t
  val of_spec : spec -> t
  val render : t -> name:string -> string

end

module RenderEnum : (RenderT with type spec := Sp.Enum_spec.t) = struct

  type t = Sp.Enum_spec.t

  let of_spec spec = spec

  let render (t:t) ~name =
    let t_str =
      let lns = t.enums |> List.map (fun (e:Sp.Enum_spec.enum_val) -> e.safe_name) |> String.concat " | " in
      Printf.sprintf "type t = %s " lns
    in

    let enc_t_s =
      let lns = t.enums |> List.map (fun (e:Sp.Enum_spec.enum_val) -> Printf.sprintf "%s -> \"%s\"" e.safe_name e.dirty_name) in
      String.concat " | " lns
    in
    let enc_s_t =
      let lns = t.enums |> List.map (fun (e:Sp.Enum_spec.enum_val) -> Printf.sprintf "\"%s\" -> %s" e.dirty_name e.safe_name) in
      String.concat " | " (lns @ [Printf.sprintf "_ -> failwith \"%s\"" name])
    in
    let enc_str = Printf.sprintf
        "let enc = \n \
         let open Data_encoding in \n \
         conv \n \
         (function %s)\n \
         (function %s)\n \
         string" enc_t_s enc_s_t
    in
    Printf.sprintf "module %s = struct \n%s\n \n%s\n \nend\n" name t_str enc_str


end

module RenderObjectField = struct

  type t = Sp.Field_spec.t

  let render_t (t:t) =
    Printf.sprintf "%s: %s%s;" t.safe_name t.type_ (if t.required then "" else " option")

  let render_enc (t:t) =
    Printf.sprintf "(%s \"%s\" %s)" (if t.required then "req" else "opt") t.dirty_name t.enc_

  let render_arg (t:t) =
    Printf.sprintf "%s%s" (if t.required then "~" else "?") t.safe_name

end

module RenderObject : (RenderT with type spec := Sp.Obj_spec.t) = struct

  type t = Sp.Obj_spec.t

  let of_spec spec = spec

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
      let ss = t.fields |> List.map (fun (f:Sp.Field_spec.t) -> f.dirty_name) |> String.concat ", " in
      if List.length t.fields = 1 then Printf.sprintf "%s" ss else Printf.sprintf "(%s)" ss
    in
    let args_str =
      t.fields |> List.map RenderObjectField.render_arg |> String.concat " "
    in

    let enc_str = Printf.sprintf
        "let enc = \n \
         let open Data_encoding in \n \
         conv \n \
         (fun %s -> %s)\n \
         (fun %s -> %s)\n \
         %s" rec_str tup_str tup_str rec_str enc_obj_str
    in
    let make_str = Printf.sprintf
        "let make %s () = \n%s" args_str rec_str
    in
    Printf.sprintf "module %s = struct \n%s\n \n%s\n \n%s\n \nend\n" name t_str enc_str make_str

    (* TODO cyclic objects and large objects >10 fields *)


end

let _EMPTY_OBJECT = "EmptyObject"


module RenderRequest : (RenderT with type spec := Sp.Obj_spec.t) = struct

  type t = Sp.Obj_spec.t

  let of_spec spec = spec

  let render (t:t) ~name =
    let command = CommandHelper.strip_command name ~on:"Request" in
    match t.fields with
    | [args; _cmd] when args.required ->
      Printf.sprintf
        "module %s = MakeRequest (%s) (%s)"
        name
        command
        args.module_name
    | [args; _cmd] ->
      Printf.sprintf
        "module %s = MakeRequest_optionalArgs (%s) (%s)"
        name
        command
        args.module_name
    | [_cmd] ->
      Printf.sprintf
        "module %s = MakeRequest_optionalArgs (%s) (%s)"
        name
        command
        _EMPTY_OBJECT
    | [] ->
      Printf.sprintf
        "module %s = MakeRequest_optionalArgs (%s) (%s)"
        name
        command
        _EMPTY_OBJECT
    | _ -> assert false

end

module RenderResponse : (RenderT with type spec := Sp.Obj_spec.t) = struct

  type t = Sp.Obj_spec.t

  let of_spec spec = spec

  let render (t:t) ~name =
    let command = CommandHelper.strip_command name ~on:"Response" in
    match t.fields with
    | [body] when body.required ->
      Printf.sprintf
        "module %s = MakeResponse (%s) (%s)"
        name
        command
        body.module_name
    | [body] ->
      Printf.sprintf
        "module %s = MakeResponse_optionalBody (%s) (%s)"
        name
        command
        body.module_name
    | [] ->
      Printf.sprintf
        "module %s = MakeResponse_optionalBody (%s) (%s)"
        name
        command
        _EMPTY_OBJECT
    | _ -> assert false

end

module RenderEvent : (RenderT with type spec := Sp.Obj_spec.t) = struct

  type t = Sp.Obj_spec.t

  let of_spec spec = spec

  let render (t:t) ~name =
    let event = EventHelper.strip_event name in
    match t.fields with
    | [body; _ev] when body.required ->
      Printf.sprintf
        "module %s = MakeEvent (%s) (%s)"
        name
        event
        body.module_name
    | [body; _ev] ->
      Printf.sprintf
        "module %s = MakeEvent_optionalBody (%s) (%s)"
        name
        event
        body.module_name
    | [_ev] ->
      Printf.sprintf
        "module %s = MakeEvent_optionalBody (%s) (%s)"
        name
        event
        _EMPTY_OBJECT
    | [] ->
      Printf.sprintf
        "module %s = MakeEvent_optionalBody (%s) (%s)"
        name
        event
        _EMPTY_OBJECT
    | _ -> assert false

end
