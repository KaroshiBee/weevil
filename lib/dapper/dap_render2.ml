
module Sp = Dap_specs
module CommandHelper = Dap_dfs.CommandHelper
module EventHelper = Dap_dfs.EventHelper
module Dfs = Dap_dfs.Dfs

(* want to be able to easily add new derivings *)

type field = {
  field_name : string
; field_dirty_name : string
; field_type : [ `Builtin of builtin | `Struct of struct_ ]
; field_presence : [`Opt | `Req ]
; field_index : int
} [@@deriving show, eq]

and struct_ = {
  struct_name : string (* what the struct would be called ie Thing *)
; struct_t : string (* what 't' would be called i.e. for Thing.t *)
; struct_fields : field list
} [@@deriving show, eq ]

and builtin = {
  builtin_name : string
} [@@deriving show, eq ]

let unit_ = {
  builtin_name="unit"
}

let test_data () =
  let struct_name = "Thing" in
  let struct_t = "t" in
  let struct_fields = [
    { field_name="variables";
      field_dirty_name="_variables_";
      field_type=`Struct {struct_name="Irmin.Contents.Json_value";
                          struct_t="t";
                          struct_fields=[]};
      field_presence=`Opt;
      field_index=2 };
    { field_name="format";
      field_dirty_name="format_";
      field_type=`Builtin {builtin_name="string"};
      field_presence=`Req;
      field_index=1 };
    { field_name="sendTelemetry";
      field_dirty_name="sendTelemetry_";
      field_type=`Builtin {builtin_name="bool"};
      field_presence=`Opt;
      field_index=3 };
  ]
  in
  {struct_name; struct_t; struct_fields}

module type PP_struct = sig
  val pp : Format.formatter -> struct_ -> unit
end

(* NOTE these @@ are awkard when used directly in format string *)
let deriving_str = "@@deriving"

module Stanza_t_sig : PP_struct = struct

  (* TODO compose in the deriving irmin *)
  let pp =
    Fmt.of_to_string (function {struct_t; _} ->
        Fmt.str "type %s [%s irmin]" struct_t deriving_str
      )

  let%expect_test "Check Stanza_t_sig" =
    let grp = test_data () in
    print_endline @@ Format.asprintf "%a" pp grp;
    [%expect {|type t [@@deriving irmin] |}]

end

module Stanza_t_struct : PP_struct = struct

  (* type t = { *)
  (*   id : int; *)
  (*   format : string; *)
  (*   variables : Irmin.Contents.Json_value.t option; *)
  (*   sendTelemetry : bool option; *)
  (*   showUser : bool option; *)
  (*   url : string option; *)
  (*   urlLabel : string option; *)
  (* } *)
  (* [@@deriving irmin] *)

  let pp_field =
    Fmt.of_to_string (function {field_name; field_type; field_presence; _} ->
      let presence = match field_presence with | `Opt -> "option" | `Req -> "" in
      match field_type with
      | `Builtin {builtin_name} ->
        Fmt.str "%s : %s %s" field_name builtin_name presence
      | `Struct {struct_name; struct_t; _} ->
        Fmt.str "%s : %s.%s %s" field_name struct_name struct_t presence
      )

  let pp =
    Fmt.of_to_string (function {struct_fields; struct_t; _} ->
        let fields =
          struct_fields
          |> List.sort (fun x y -> compare x.field_index y.field_index)
        in
        let pp_fields = Fmt.list ~sep:(Fmt.any ";\n") pp_field in
        Fmt.str "type %s = {\n%a\n}\n[%s irmin]" struct_t pp_fields fields deriving_str
      )

  let%expect_test "Check Stanza_struct" =
    let grp = test_data () in
    print_endline @@ Format.asprintf "%a" pp grp;
    [%expect {|
      type t = {
      format : string ;
      variables : Irmin.Contents.Json_value.t option;
      sendTelemetry : bool option
      }
      [@@deriving irmin] |}]

end

module Stanza_make_sig : PP_struct = struct
(*
   e.g.
     val make :
      id:int ->
      format:string ->
      ?variables:Irmin.Contents.Json_value.t ->
      ?sendTelemetry:bool ->
      ?showUser:bool ->
      ?url:string ->
      ?urlLabel:string ->
      unit ->
      t
*)
  let pp_field =
    Fmt.of_to_string (function {field_name; field_type; field_presence; _} ->
      let presence = match field_presence with | `Opt -> "?" | `Req -> "" in
      match field_type with
      | `Builtin {builtin_name} ->
        Fmt.str "%s%s : %s" presence field_name builtin_name
      | `Struct {struct_name; struct_t; _} ->
        Fmt.str "%s%s : %s.%s" presence field_name struct_name struct_t
      )

  let pp =
    Fmt.of_to_string (function {struct_fields; struct_t; _} ->
        let fields =
          struct_fields
          |> List.sort (fun x y -> compare x.field_index y.field_index)
        in
        let pp_fields = Fmt.list ~sep:(Fmt.any " -> \n") pp_field in
        Fmt.str "val make : \n%a -> \nunit -> \n%s" pp_fields fields struct_t
      )

  let%expect_test "Check Stanza_make_sig" =
    let grp = test_data () in
    print_endline @@ Format.asprintf "%a" pp grp;
    [%expect {|
      val make :
      format : string ->
      ?variables : Irmin.Contents.Json_value.t ->
      ?sendTelemetry : bool ->
      unit ->
      t |}]

end

module Stanza_make_struct : PP_struct = struct
(*
   e.g.
    let make ~id ~format ?variables ?sendTelemetry ?showUser ?url ?urlLabel () =
      {id; format; variables; sendTelemetry; showUser; url; urlLabel}
*)
  let pp_field_upper =
    Fmt.of_to_string (function {field_name; field_presence; _} ->
        let presence = match field_presence with | `Opt -> "?" | `Req -> "~" in
        Fmt.str "%s%s" presence field_name
      )

  let pp_field_lower =
    Fmt.of_to_string (function {field_name; _} ->
        Fmt.str "%s" field_name
      )

  let pp =
    Fmt.of_to_string (function {struct_fields; _} ->
        let all_fields =
          struct_fields
          |> List.sort (fun x y -> compare x.field_index y.field_index)
        in
        let pp_upper = Fmt.list ~sep:(Fmt.any " ") pp_field_upper in
        let pp_lower = Fmt.list ~sep:(Fmt.any "; ") pp_field_lower in
        Fmt.str "let make %a () =\n{%a}\n" pp_upper all_fields pp_lower all_fields
      )

  let%expect_test "Check Stanza_make_struct" =
    let grp = test_data () in
    print_endline @@ Format.asprintf "%a" pp grp;
    [%expect {|
      let make ~format ?variables ?sendTelemetry () =
      {format; variables; sendTelemetry} |}]

end

module Stanza_getters_sig : PP_struct = struct
(*
   e.g.
    val id : t -> int

    val format : t -> string

    val variables : t -> Irmin.Contents.Json_value.t option
*)
  let pp_field ~struct_t =
    Fmt.of_to_string (function {field_name; field_type; field_presence; _} ->
        let presence = match field_presence with | `Opt -> "option" | `Req -> "" in
        match field_type with
        | `Builtin {builtin_name} ->
          Fmt.str "val %s : %s -> %s %s" field_name struct_t builtin_name presence
        | `Struct {struct_name; struct_t; _} ->
          Fmt.str "val %s : %s -> %s.%s %s" field_name struct_t struct_name struct_t presence
      )

  let pp =
    Fmt.of_to_string (function {struct_fields; struct_t; _} ->
        let all_fields =
          struct_fields
          |> List.sort (fun x y -> compare x.field_index y.field_index)
        in
        let pp_fields = Fmt.list ~sep:(Fmt.any "\n\n") (pp_field ~struct_t) in
        Fmt.str "%a" pp_fields all_fields
      )

  let%expect_test "Check Stanza_getter_sig" =
    let grp = test_data () in
    print_endline @@ Format.asprintf "%a" pp grp;
    [%expect {|
      val format : t -> string

      val variables : t -> Irmin.Contents.Json_value.t option

      val sendTelemetry : t -> bool option |}]

end

module Stanza_getter_struct : PP_struct = struct
(*
   e.g.
    let id t = t.id

    let format t = t.format

    let variables t = t.variables
*)
  let pp_field ~struct_t =
    Fmt.of_to_string (function {field_name; _} ->
        Fmt.str "let %s %s = %s.%s" field_name struct_t struct_t field_name
      )

  let pp =
    Fmt.of_to_string (function {struct_fields; struct_t; _} ->
        let all_fields =
          struct_fields
          |> List.sort (fun x y -> compare x.field_index y.field_index)
        in
        let pp_fields = Fmt.list ~sep:(Fmt.any "\n\n") (pp_field ~struct_t) in
        Fmt.str "%a" pp_fields all_fields
      )

  let%expect_test "Check Stanza_getter_struct" =
    let grp = test_data () in
    print_endline @@ Format.asprintf "%a" pp grp;
    [%expect {|
      let format t = t.format

      let variables t = t.variables

      let sendTelemetry t = t.sendTelemetry |}]

end

  (* Stanza.t represents the stanzas for
   part of a type t, part of an encoding,
   part of a make func, part of the getter.

   These stanzas can be for the sig decl or the struct decl
   The pps dont have to do anything if they dont need to
*)
module type Stanza = sig
  type t
  type struct_ (* something that represents another json object *)

  type presence = Opt | Req
  val name : t -> string
  val dirty_name : t -> string
  val type_ : t -> struct_

  (* pp the part that will be in the type t decl *)
  val pp_t : Format.formatter -> t -> unit
  (* pp the part that will go in the enc objN decl *)
  val pp_enc : Format.formatter -> t -> unit
  (* the bit in the make func *)
  val pp_make : Format.formatter -> t -> unit
  (* pp the part that will give you access func *)
  val pp_getter : Format.formatter -> t -> unit

end

module type Object = sig
  type t
  type context (* other modules by name *)
  type deriving (* something to describe a deriving clause e.g. irmin, show, eq etc *)
  type stanza (* something describing a field { name, type, 'Req|'Opt } *)

  val add_deriving : t -> context:context -> deriving -> t
  val add_required : t -> context:context -> stanza -> t
  val add_optional : t -> context:context -> stanza -> t

  (* pp the whole type t decl *)
  val pp_t : Format.formatter -> t -> unit
  (* pp the whole enc objN decl *)
  val pp_enc : Format.formatter -> t -> unit
  (* the make func *)
  val pp_make : Format.formatter -> t -> unit
  (* pp the access funcs *)
  val pp_getter : Format.formatter -> t -> unit

  val pp : Format.formatter -> t -> unit

end

module Render_output = struct
  type msg = {
        modstr: string;
        tystr: string;
        ctor: string;
        ctorstr: string;
        genstr: string;
        encstr: string;

      }
  type mlmli = {ml: string; mli:string;}

  type t = [
    | `Message of msg
    | `MlMli of mlmli
  ]

  let make_msg ?(tystr="") ?(ctor="") ?(ctorstr="") ?(genstr="") ?(encstr="") modstr =
    `Message {modstr; tystr; ctor; ctorstr; genstr; encstr}

  let make_mlmli ml mli =
    `MlMli {ml; mli}

end


module type RenderT = sig
  type spec
  type t
  val of_spec : spec -> t
  val render : t -> name:string -> internal_with_sig:bool -> Render_output.t

end

module RenderEnum = struct (* : (RenderT with type spec := Sp.Enum_spec.t) = struct *)

  type t = Sp.Enum_spec.t

  let of_spec spec = spec

  let pp_names ppf names =
    List.iter (fun name ->
        Format.fprintf ppf "@[ | %s@]@." name
      ) names

  let pp_t ppf (t:t) =
    let names =
      List.map (fun (e:Sp.Enum_spec.enum_val) -> e.safe_name) t.enums
    in
    let names =
      if t.suggested then
        names @ ["Other of string"]
      else
        names
    in
    Format.fprintf ppf "type %s = %a [@@deriving qcheck, eq]" "t" pp_names names

  let%expect_test "Check RenderEnum.pp_t" =
    let spec = Sp.Enum_spec.of_path
        ~dirty_name:"Bad name"
        ~path:[`Field "/definitions"; `Field "something"]
        ~dirty_names:["enum1"; "Enum2"; "enUM 3"]
        ~suggested:true
        () in
    let t = of_spec spec in
    print_endline @@ Format.asprintf "%a" pp_t t;
    [%expect {|
      type t =  | Enum1
       | Enum2
       | EnUM_3
       | Other of string
       [@deriving qcheck, eq]|}]

  let pp_safe_dirty ppf names =
    List.iter (function
        | (safe_name, `Str dirty_name) ->
          Format.fprintf ppf " | %s -> \"%s\"" safe_name dirty_name
        | (safe_name, `Raw dirty_name) ->
          Format.fprintf ppf " | %s -> %s" safe_name dirty_name
        ) names

  let pp_enc_2_str ppf (t:t) =
    let names =
      List.map (fun (e:Sp.Enum_spec.enum_val) -> (e.safe_name, `Str e.dirty_name)) t.enums
    in
    let names =
      if t.suggested then
        names @ [("Other s", `Raw "s")]
      else
        names
    in
    Format.fprintf ppf "%a" pp_safe_dirty names

  let%expect_test "Check RenderEnum.pp_enc" =
    let spec = Sp.Enum_spec.of_path
        ~dirty_name:"Bad name"
        ~path:[`Field "/definitions"; `Field "something"]
        ~dirty_names:["enum1"; "Enum2"; "enUM 3"]
        ~suggested:true
        () in
    let t = of_spec spec in
    print_endline @@ Format.asprintf "%a" pp_enc_2_str t;
    [%expect {|
       | Enum1 -> "enum1" | Enum2 -> "Enum2" | EnUM_3 -> "enUM 3" | Other s -> s|}]

  let pp_dirty_safe ppf names =
    List.iter (function
        | (safe_name, `Str dirty_name) ->
          Format.fprintf ppf " | \"%s\" -> Ok %s" dirty_name safe_name
        | (safe_name, `Raw dirty_name) ->
          Format.fprintf ppf " | _ as %s -> Ok (Other %s)" dirty_name safe_name

        ) names

    let enc_s_t (t:t) name =
      let lns = t.enums |> List.map (fun (e:Sp.Enum_spec.enum_val) -> Printf.sprintf "\"%s\" -> Ok %s" e.dirty_name e.safe_name) in
      let lns = if t.suggested then lns @ ["_ as s -> Ok (Other s)"] else lns @ [Printf.sprintf "_ -> Error \"%s\"" name] in
      String.concat " | " lns

end
(*   let render (t:t) ~name ~internal_with_sig:_ = *)
(*     in *)

(*     in *)
(*     in *)
(*     let enc_str = Printf.sprintf *)
(*         "let enc = \n \ *)
(*          let open Data_encoding in \n \ *)
(*          conv_with_guard \n \ *)
(*          (function %s)\n \ *)
(*          (function %s)\n \ *)
(*          string" enc_t_s enc_s_t *)
(*     in *)
(*     Render_output.make_msg @@ *)
(*     Printf.sprintf *)
(*       "(\* dont bother with a sig for enums, the inferred one is fine *\)\n \ *)
(*        module %s = struct \n \ *)
(*        %s\n\n \ *)
(*        %s\n\n \ *)
(*        end\n" name t_str enc_str *)

(* end *)


(* module RenderEnumWithPhantoms : (RenderT with type spec := Sp.Enum_spec.t) = struct *)

(*   type t = Sp.Enum_spec.t *)

(*   let of_spec spec = spec *)

(*   let render_ml (t:t) ~name = *)
(*     let t_str = *)
(*       let lns = t.enums |> List.map (fun (e:Sp.Enum_spec.enum_val) -> e.safe_name |> String.capitalize_ascii) |> String.concat " | " in *)
(*       Printf.sprintf *)
(*         "(\* NOTE autogenerated, do not manually edit *\)\n \ *)
(*          type v = %s [@@deriving eq]\n\n \ *)
(*          type 'a t = v" lns *)
(*     in *)

(*     let types_str = *)
(*       t.enums |> List.map (fun (e:Sp.Enum_spec.enum_val) -> *)
(*           let s = e.safe_name |> String.uncapitalize_ascii in *)
(*           Printf.sprintf "type %s" s *)
(*         ) |> String.concat "\n" *)
(*     in *)

(*     let eq_str = *)
(*       "let equal = equal_v" *)
(*     in *)

(*     let ctors_str = *)
(*       t.enums |> List.map (fun (e:Sp.Enum_spec.enum_val) -> *)
(*           let l = e.safe_name |> String.uncapitalize_ascii in *)
(*           let u = l |> String.capitalize_ascii in *)
(*           Printf.sprintf "let %s : %s t = %s" l l u *)
(*         ) |> String.concat "\n" *)
(*     in *)

(*     let func_str reversed = *)
(*       let lns = t.enums |> List.map (fun (e:Sp.Enum_spec.enum_val) -> *)
(*           let u = e.safe_name |> String.capitalize_ascii in *)
(*           let l = e.safe_name |> String.uncapitalize_ascii in *)
(*           let d = e.dirty_name in *)
(*           if not reversed then *)
(*             Printf.sprintf "| (%s : %s t) -> \"%s\"" u l d *)
(*           else *)
(*             Printf.sprintf "| \"%s\" -> (%s : %s t)" d u l *)
(*         ) |> String.concat "\n" *)
(*       in *)
(*       let lns = if reversed then *)
(*           lns ^ (Printf.sprintf "| _ -> failwith \"%s\"\n" name) *)
(*         else *)
(*           lns *)
(*       in *)
(*       lns *)
(*     in *)


(*     let f_str = *)
(*       Printf.sprintf "let to_string (type a) : a t -> string = function %s" @@ func_str false *)
(*     in *)

(*     let g_str = *)
(*       Printf.sprintf "let from_string (type a) : string -> a t = function %s" @@ func_str true *)
(*     in *)

(*     let enc_str = {| *)
(* let enc ~value = *)
(*   let open Data_encoding in *)
(*   let to_str = to_string in *)
(*   let from_str = *)
(*     let sentinal = to_string value in *)
(*     function *)
(*     | s when s = sentinal -> *)
(*       Ok (from_string s) *)
(*     | _  as s -> *)
(*       let err = Printf.sprintf "expected '%s', got '%s'" sentinal s in *)
(*       Error err *)
(*   in *)
(*   conv_with_guard *)
(*     to_str from_str string *)
(*       |} *)

(*     in *)
(*     [t_str; types_str; eq_str; ctors_str; f_str; g_str; enc_str; ] |> String.concat "\n\n" *)

(*   let render_mli (t:t) = *)
(*     let types_str = *)
(*       let lns = t.enums |> List.map (fun (e:Sp.Enum_spec.enum_val) -> *)
(*           let s = e.safe_name |> String.uncapitalize_ascii in *)
(*           Printf.sprintf "type %s" s *)
(*         ) |> String.concat "\n" *)
(*       in *)
(*       Printf.sprintf *)
(*         "(\* NOTE autogenerated, do not manually edit *\)\n \ *)
(*          type 'a t\n%s" lns *)
(*     in *)

(*     let eq_str = *)
(*       "val equal : 'a t -> 'b t -> bool" *)
(*     in *)

(*     let ctors_str = *)
(*       t.enums |> List.map (fun (e:Sp.Enum_spec.enum_val) -> *)
(*           let l = e.safe_name |> String.uncapitalize_ascii in *)
(*           Printf.sprintf "val %s : %s t" l l *)
(*         ) |> String.concat "\n" *)
(*     in *)

(*     let funcs_str = *)
(*       "val to_string : 'a t -> string\n\nval from_string : string -> 'a t" *)
(*     in *)

(*     let enc_str = *)
(*       "val enc : value:'a t -> 'a t Data_encoding.t" *)
(*     in *)

(*     [types_str; eq_str; ctors_str; funcs_str; enc_str; ] |> String.concat "\n\n" *)


(*   let render (t:t) ~name ~internal_with_sig:_ = *)
(*     let ml = render_ml t ~name in *)
(*     let mli = render_mli t in *)
(*     Render_output.make_mlmli ml mli *)

(* end *)


(* module RenderObjectField = struct *)

(*   type t = Sp.Field_spec.t *)

(*   let sig_typ_str ~required ~seq safe_name type_ = *)
(*     let s = Printf.sprintf (if seq then "%s list" else "%s") type_ in *)
(*     Printf.sprintf (if required then "%s:%s" else "?%s:%s") safe_name s *)

(*   let render_sig = function *)
(*     | Sp.Field_spec.{ safe_name; type_; required; cyclic; seq; _ } when not cyclic -> *)
(*       sig_typ_str ~required ~seq safe_name type_ *)
(*     | Sp.Field_spec.{ safe_name; required; seq; _ } -> *)
(*       sig_typ_str ~required ~seq safe_name "t" *)

(*   let typ_str ~with_qcheck_annot ~with_eq_annot ~required ~seq type_ = *)
(*     (\* TODO might be list option rather than option list *\) *)
(*     let s = Printf.sprintf (if seq then "%s list" else "%s") type_ in *)
(*     let s = Printf.sprintf (if required then "%s" else "%s option") s in *)
(*     let qcheck_annot = *)
(*       if with_qcheck_annot then *)
(*         Dap_base.Gen.convert_ocaml_type_str ~as_annot:true s |> Option.value ~default:"" *)
(*       else "" *)
(*     in *)
(*     let eq_annot = *)
(*       if with_eq_annot then *)
(*         Dap_base.Eq.convert_ocaml_type_str ~as_annot:true s |> Option.value ~default:"" *)
(*       else "" *)
(*     in *)
(*     Printf.sprintf "(%s %s %s)" s qcheck_annot eq_annot *)

(*   let req_enc_str ~required = if required then "req" else "opt" *)
(*   let seq_enc_str ~seq name = if seq then Printf.sprintf "(list %s)" name else name *)

(*   let render_accessor_sig = function *)
(*     | Sp.Field_spec.{ safe_name; type_; required; cyclic; seq; _ } when not cyclic -> *)
(*         Printf.sprintf "val %s : t -> %s" safe_name @@ typ_str ~with_qcheck_annot:false ~with_eq_annot:false ~required ~seq type_ *)
(*     | Sp.Field_spec.{ safe_name; required; seq; _ } -> *)
(*         Printf.sprintf "val %s : t -> %s" safe_name @@ typ_str ~with_qcheck_annot:false ~with_eq_annot:false ~required ~seq "t" *)

(*   let render_t = function *)
(*     (\* for the type t decl *)
(*        NOTE if it is cyclic field then hardcode type name to 't' *\) *)
(*     | Sp.Field_spec.{ safe_name; type_; required; cyclic; seq; _ } when not cyclic -> *)
(*       Printf.sprintf "%s: %s;" safe_name @@ typ_str ~with_qcheck_annot:true ~with_eq_annot:true ~required ~seq type_ *)
(*     | Sp.Field_spec.{ safe_name; required; seq; _ } -> *)
(*       Printf.sprintf "%s: %s;" safe_name @@ typ_str ~with_qcheck_annot:false ~with_eq_annot:true ~required ~seq "t" *)

(*   let render_enc = function *)
(*     (\* for the encoding function *)
(*        NOTE if it is a cyclic field then hardcode the enc name to 'e' *\) *)
(*     | Sp.Field_spec.{ dirty_name; enc_; required; cyclic; seq; _ } when not cyclic -> *)
(*       Printf.sprintf "(%s \"%s\" %s)" (req_enc_str ~required) dirty_name (seq_enc_str ~seq enc_) *)
(*     | Sp.Field_spec.{ dirty_name; required; seq; _ } -> *)
(*       Printf.sprintf "(%s \"%s\" %s)" (req_enc_str ~required) dirty_name (seq_enc_str ~seq "e") *)

(*   let render_arg (t:t) = *)
(*     (\* for the make function *\) *)
(*     Printf.sprintf "%s%s" (if t.required then "~" else "?") t.safe_name *)

(*   let render_accessor = function *)
(*     | Sp.Field_spec.{ safe_name; _ } -> *)
(*         Printf.sprintf "let %s t = t.%s" safe_name safe_name *)

(*   let render_sub_accessor ~tt ~modname ~t = function *)
(*     | Sp.Field_spec.{ safe_name; _ } -> *)
(*         Printf.sprintf "let %s %s = %s.%s %s" safe_name tt modname safe_name t *)

(* end *)


(* module RenderObject : (RenderT with type spec := Sp.Obj_spec.t) = struct *)

(*   type t = Sp.Obj_spec.t *)

(*   let of_spec = function *)
(*     | Sp.Obj_spec.{fields; _} as spec when 10 >= List.length fields -> *)
(*       spec *)
(*     | _ -> failwith "Use RenderLargeObject" *)

(*   let render (t:t) ~name ~internal_with_sig = *)
(*     let sig_str = *)
(*       let make_sig = t.fields |> List.map RenderObjectField.render_sig |> String.concat " -> " in *)
(*       let accessors_sig = t.fields |> List.map RenderObjectField.render_accessor_sig |> String.concat "\n" in *)
(*       Printf.sprintf *)
(*         "type t \n \ *)
(*          val equal : t -> t -> bool \n \ *)
(*          val enc : t Data_encoding.t \n \ *)
(*          val gen : t QCheck.Gen.t \n \ *)
(*          val arb : t QCheck.arbitrary \n \ *)
(*          val make : %s -> unit -> t \n \ *)
(*          %s" *)
(*         make_sig accessors_sig *)
(*     in *)
(*     let t_str = *)
(*       let lns = t.fields |> List.map RenderObjectField.render_t |> String.concat "\n" in *)
(*       (\* cyclic qcheck needs special gen/arb function so dont derive ppx qcheck - assumes only one cyclic param *\) *)
(*       Printf.sprintf "type t = { %s }" lns ^ " " ^ if t.is_cyclic then "[@@deriving eq]" else "[@@deriving qcheck, eq]" *)
(*     in *)

(*     let gen_arb_str = *)
(*       if not t.is_cyclic then "" else *)
(*         let safe_names = *)
(*           t.fields *)
(*           |> List.map (fun Sp.Field_spec.{safe_name; _} -> safe_name) *)
(*         in *)
(*         let newlined_gennames = *)
(*           t.fields *)
(*           |> List.map (fun Sp.Field_spec.{type_; required; seq; cyclic; _} -> *)
(*               if cyclic then "t" else (\* this 't' is the name of the arg passed to the fix func below *\) *)
(*                 let s = Printf.sprintf (if seq then "%s list" else "%s") type_ in *)
(*                 let s = Printf.sprintf (if required then "%s" else "%s option") s in *)
(*                 let s = Dap_base.Gen.convert_ocaml_type_str ~as_annot:false s in *)
(*                 let s = Option.value s ~default:( *)
(*                     match (seq, required) with *)
(*                     | true, true -> "(list " ^ type_ ^ ")" *)
(*                     | true, false -> "(option @@ list " ^ type_ ^ ")" *)
(*                     | false, true -> type_ *)
(*                     | false, false -> "(option " ^ type_ ^ ")" *)
(*                   ) in *)
(*                 let s = Str.global_replace (Str.regexp_string ".t") ".gen" s in *)
(*                 s *)
(*             ) *)
(*           |> String.concat "\n" *)
(*         in *)
(*         let comma_safenames = *)
(*           safe_names |> String.concat ", " *)
(*         in *)
(*         let colon_safenames = *)
(*           safe_names |> String.concat "; " *)
(*         in *)

(*         let cyclic_safename = *)
(*           t.fields *)
(*           |> List.filter_map (fun Sp.Field_spec.{safe_name; cyclic; _} -> if cyclic then Some safe_name else None) *)
(*           |> (fun ls -> List.nth_opt ls 0) *)
(*           |> Option.get *)
(*         in *)
(*         Printf.sprintf *)
(*           "let gen = QCheck.Gen.(sized @@ fix (fun self n -> \n \ *)
(*            let basecase = oneofl [None; Some []] in \n \ *)
(*            let _gen_t = *)
(*             fun t -> *)
(*               let gg = *)
(*                 tup%d *)
(*                   %s *)
(*               in *)
(*               map (fun ( %s ) -> { %s }) gg *)
(*           in *)
(*           match n with *)
(*           | 0 -> _gen_t basecase *)
(*           | n -> *)
(*             frequency *)
(*               [ *)
(*                 (1, _gen_t basecase); *)
(*                 (1, let t = map (fun {%s; _} -> %s) @@ self (n / 2) in _gen_t t); *)
(*               ] *)
(*         )) \n \ *)
(*            let arb = QCheck.make gen \n " *)
(*           (List.length t.fields) newlined_gennames comma_safenames colon_safenames cyclic_safename cyclic_safename *)
(*     in *)
(*     let enc_obj_str = *)
(*       let lns = t.fields |> List.map RenderObjectField.render_enc |> String.concat "\n" in *)
(*       Printf.sprintf "(obj%d\n%s)\n" (List.length t.fields) lns *)
(*     in *)
(*     let rec_str = *)
(*       let ss = t.fields |> List.map (fun (f:Sp.Field_spec.t) -> f.safe_name) |> String.concat "; " in *)
(*       Printf.sprintf "{%s}" ss *)
(*     in *)
(*     let tup_str = *)
(*       let ss = t.fields |> List.map (fun (f:Sp.Field_spec.t) -> f.safe_name) |> String.concat ", " in *)
(*       if List.length t.fields = 1 then Printf.sprintf "%s" ss else Printf.sprintf "(%s)" ss *)
(*     in *)
(*     let args_str = *)
(*       t.fields |> List.map RenderObjectField.render_arg |> String.concat " " *)
(*     in *)

(*     let enc_str = *)
(*       let fmt = if t.is_cyclic then *)
(*           Printf.sprintf *)
(*             "let enc = \n \ *)
(*              let open Data_encoding in \n \ *)
(*              mu \"%s.t\" \n \ *)
(*              ( fun e -> \n \ *)
(*              conv \n \ *)
(*              (fun %s -> %s)\n \ *)
(*              (fun %s -> %s)\n \ *)
(*              %s)" *)
(*           else Printf.sprintf *)
(*             "let enc = \n \ *)
(*              let open Data_encoding in \n \ *)
(*              (\* %s.t *\) *)
(*          conv \n \ *)
(*              (fun %s -> %s)\n \ *)
(*              (fun %s -> %s)\n \ *)
(*              %s" in *)
(*       fmt name rec_str tup_str tup_str rec_str enc_obj_str *)
(*     in *)
(*     let make_str = Printf.sprintf *)
(*         "let make %s () = \n%s" args_str rec_str *)
(*     in *)
(*     let accessors_str = *)
(*       t.fields |> List.map RenderObjectField.render_accessor |> String.concat "\n" in *)

(*     Render_output.make_msg @@ ( *)
(*       let mod_str = *)
(*         if internal_with_sig then *)
(*           Printf.sprintf "module %s : sig \n%s\nend" name sig_str *)
(*         else *)
(*           Printf.sprintf "module %s" name *)
(*       in *)
(*       Printf.sprintf *)
(*       "%s = struct \n \ *)
(*          %s\n\n \ *)
(*          %s\n\n \ *)
(*          %s\n\n \ *)
(*          %s\n\n \ *)
(*          %s\n\n \ *)
(*          end\n" mod_str t_str gen_arb_str enc_str make_str accessors_str *)
(*   ) *)

(* end *)


(* module RenderLargeObject : (RenderT with type spec := Sp.Obj_spec.t) = struct *)
(*   (\* NOTE objects with more than 10 fields need to be built with merge_obj *\) *)

(*   type t = { *)
(*     spec: Sp.Obj_spec.t; *)
(*     ngroups: int; *)
(*     nleftover: int; *)
(*   } *)

(*   let of_spec = function *)
(*     | Sp.Obj_spec.{fields; is_cyclic; _} as spec when 10 < List.length fields -> *)
(*       if is_cyclic then failwith "TODO cyclic big objects" else ( *)
(*         let n = List.length fields in *)
(*         let ngroups = n / 10 in *)
(*         let nleftover = n mod 10 in *)
(*         {spec; ngroups; nleftover} *)
(*       ) *)
(*     | _ -> failwith "Use RenderObject" *)

(*   let render {spec; ngroups; nleftover} ~name ~internal_with_sig:_ = *)
(*     let fields_arr = Array.of_list spec.fields in *)
(*     let n = Array.length fields_arr in *)
(*     assert (nleftover + (ngroups * 10) = n); *)

(*     let modstrs = *)
(*       let ss = *)
(*         List.init ngroups succ *)
(*         |> List.fold_left (fun acc i -> *)
(*             let i = 10 * (i-1) in *)
(*             let fields = Array.sub fields_arr i 10 |> Array.to_list in *)
(*             let dirty_name = Printf.sprintf "%s_%d" spec.safe_name i in *)
(*             let spec = Sp.Obj_spec.of_path ~dirty_name ~path:[] ~fields () in *)
(*             let name = Printf.sprintf "%s_%d" name i in *)
(*             (\* these internal defn dont need sigs *\) *)
(*             let modstr = match RenderObject.(of_spec spec |> render ~name ~internal_with_sig:false) with *)
(*               | `Message msg -> Render_output.(msg.modstr) *)
(*               | _ -> assert false *)
(*             in *)
(*             (name, modstr, fields) :: acc *)
(*           ) [] in *)

(*       (\* leftover fields *\) *)
(*       let i = 10 * ngroups in *)
(*       let fields = Array.sub fields_arr i nleftover |> Array.to_list in *)
(*       let dirty_name = Printf.sprintf "%s_%d" spec.safe_name i in *)
(*       let spec = Sp.Obj_spec.of_path ~dirty_name ~path:[] ~fields () in *)
(*       let name = Printf.sprintf "%s_%d" name i in *)
(*       (\* these internal defn dont need sigs *\) *)
(*       let modstr = match RenderObject.(of_spec spec |> render ~name ~internal_with_sig:false ) with *)
(*         | `Message msg -> Render_output.(msg.modstr) *)
(*         | _ -> assert false *)
(*       in *)
(*       List.rev @@ (name, modstr, fields) :: ss *)
(*     in *)
(*     let internal_mods = modstrs |> List.map (fun (_, modstr, _) -> modstr) |> String.concat "\n\n" in *)

(*     let rec aux_brkts ~sep = function *)
(*       | x :: [y] -> Printf.sprintf "(%s %s %s)" x sep y *)
(*       | ln :: rest -> *)
(*         let lns = aux_brkts ~sep rest in *)
(*         Printf.sprintf "(%s %s %s)" ln sep lns *)
(*       | [] -> "" *)
(*     in *)
(*     let t_str = *)
(*       Printf.sprintf "type t = %s [@@deriving qcheck, eq]" @@ aux_brkts ~sep:"*" (modstrs |> List.map (fun (nm, _, _) -> nm^".t")) *)
(*     in *)
(*     let enc_str = *)
(*       let rec aux = function *)
(*         | x :: [y] -> Printf.sprintf "merge_objs %s %s" x y *)
(*         | ln :: rest -> *)
(*           let lns = aux rest in *)
(*           Printf.sprintf "merge_objs %s @@ %s" ln lns *)
(*         | [] -> "" *)
(*       in *)
(*       let ln = aux (modstrs |> List.map (fun (nm, _, _) -> nm^".enc")) in *)
(*       Printf.sprintf *)
(*           "let enc = \n \ *)
(*            let open Data_encoding in \n \ *)
(*            %s" ln *)
(*     in *)
(*     let sig_str = *)
(*       let make_sig = spec.fields |> List.map RenderObjectField.render_sig |> String.concat " -> " in *)
(*       let accessors_sig = spec.fields |> List.map RenderObjectField.render_accessor_sig |> String.concat "\n" in *)
(*       Printf.sprintf *)
(*         "type t \n \ *)
(*          val equal : t -> t -> bool \n \ *)
(*          val enc : t Data_encoding.t \n \ *)
(*          val gen : t QCheck.Gen.t \n \ *)
(*          val arb : t QCheck.arbitrary \n \ *)
(*          val make : %s -> unit -> t \n \ *)
(*          %s" *)
(*         make_sig accessors_sig *)
(*     in *)
(*     let args_str = *)
(*       spec.fields |> List.map RenderObjectField.render_arg |> String.concat " " *)
(*     in *)
(*     let rec_str = *)
(*       let ss = *)
(*         modstrs |> List.mapi (fun i (nm, _, fields) -> *)
(*             let fields = fields |> List.map RenderObjectField.render_arg |> String.concat " " in *)
(*             Printf.sprintf "let t%d = %s.make %s () in" i nm fields *)
(*           ) |> String.concat "\n\n" in *)
(*       let tt = aux_brkts ~sep:"," (modstrs |> List.mapi (fun i _ -> Printf.sprintf "t%d" i)) in *)
(*       Printf.sprintf "%s\n\n%s" ss tt *)
(*     in *)
(*     let make_str = Printf.sprintf *)
(*         "let make %s () = \n%s" args_str rec_str *)
(*     in *)

(*     let accessors_str = *)
(*       let tt = aux_brkts ~sep:"," (modstrs |> List.mapi (fun i _ -> Printf.sprintf "_t%d" i)) in *)
(*       modstrs |> List.mapi (fun i (modname, _, fields) -> *)
(*           let t = Printf.sprintf "_t%d" i in *)
(*           fields |> List.map (RenderObjectField.render_sub_accessor ~tt ~modname ~t) |> String.concat "\n" *)
(*         ) |> String.concat "\n" *)
(*     in *)

(*     Render_output.make_msg @@ *)
(*     Printf.sprintf *)
(*       "module %s : sig \n%s\nend = struct \n \ *)
(*        %s\n\n \ *)
(*        %s\n\n \ *)
(*        %s\n\n \ *)
(*        %s\n\n \ *)
(*        %s\n\n \ *)
(*        end\n" name sig_str internal_mods t_str enc_str make_str accessors_str *)

(* end *)

(* module RenderEmptyObject : (RenderT with type spec := unit) = struct *)

(*   type t = unit *)

(*   let of_spec () = () *)

(*   let render _t ~name ~internal_with_sig:_ = *)
(*     let t_str = *)
(*       "type t = unit [@@deriving qcheck, eq]" *)
(*     in *)

(*     let enc_str = *)
(*       "let enc = Data_encoding.empty" *)
(*     in *)
(*     let make_str = *)
(*         "let make () = () " *)
(*     in *)
(*     Render_output.make_msg @@ *)
(*     Printf.sprintf *)
(*       "module %s : sig \n \ *)
(*        type t \n \ *)
(*        val equal : t -> t -> bool \n \ *)
(*        val enc : t Data_encoding.t \n \ *)
(*        val gen : t QCheck.Gen.t \n \ *)
(*        val arb : t QCheck.arbitrary \n \ *)
(*        val make : unit -> t \n \ *)
(*        end = struct \n \ *)
(*        %s\n\n \ *)
(*        %s\n\n \ *)
(*        %s\n\n \ *)
(*        end\n" name t_str enc_str make_str *)

(* end *)


(* module RenderRequest : (RenderT with type spec := Sp.Obj_spec.t) = struct *)

(*   type t = Sp.Obj_spec.t *)

(*   let of_spec spec = spec *)

(*   let render (t:t) ~name ~internal_with_sig:_ = *)
(*     let command = CommandHelper.enum_str name ~on:"Request" in *)
(*     match t.fields |> List.find_opt (fun Sp.Field_spec.{safe_name; _} -> safe_name = "arguments") with *)
(*     | Some args when args.required -> *)
(*         let ty_params = Printf.sprintf "(%s.%s, %s.t, Presence.req)" *)
(*             CommandHelper.module_name *)
(*             command *)
(*             args.module_name *)
(*         in *)
(*         Render_output.make_msg *)
(*           ~ctor:( *)
(*             Printf.sprintf *)
(*               "let %s req = %s req" *)
(*               (String.uncapitalize_ascii name) name *)
(*           ) *)
(*           ~tystr:( *)
(*             Printf.sprintf *)
(*               "| %s : %s RequestMessage.t -> %s RequestMessage.t t" *)
(*               name ty_params ty_params *)
(*           ) *)
(*           ~ctorstr:(String.uncapitalize_ascii name) *)
(*           ~genstr:( *)
(*             Printf.sprintf *)
(*               "QCheck.Gen.( \ *)
(*                map (fun (seq, arguments) -> RequestMessage.make ~seq ~command:%s.%s ~arguments ()) \ *)
(*                @@ tup2 Gen.gen_int31 %s.gen \ *)
(*                )" *)
(*               CommandHelper.module_name *)
(*               command *)
(*               args.module_name *)
(*           ) *)
(*           ~encstr:( *)
(*             Printf.sprintf *)
(*               "RequestMessage.enc %s.%s %s.enc" *)
(*               CommandHelper.module_name *)
(*               command *)
(*               args.module_name *)
(*           ) *)
(*           "" *)

(*     | Some args -> *)
(*         let ty_params = Printf.sprintf "(%s.%s, %s.t option, Presence.opt)" *)
(*             CommandHelper.module_name *)
(*             command *)
(*             args.module_name *)
(*         in *)
(*         Render_output.make_msg *)
(*           ~ctor:( *)
(*             Printf.sprintf *)
(*               "let %s req = %s req" *)
(*               (String.uncapitalize_ascii name) name *)
(*           ) *)
(*           ~tystr:( *)
(*             Printf.sprintf *)
(*               "| %s : %s RequestMessage.t -> %s RequestMessage.t t" *)
(*               name ty_params ty_params *)
(*           ) *)
(*           ~ctorstr:(String.uncapitalize_ascii name) *)
(*           ~genstr:( *)
(*             Printf.sprintf *)
(*               "QCheck.Gen.( \ *)
(*                map (fun (seq, arguments) -> RequestMessage.make_opt ~seq ~command:%s.%s ~arguments ()) \ *)
(*                @@ tup2 Gen.gen_int31 %s.gen \ *)
(*                )" *)
(*               CommandHelper.module_name *)
(*               command *)
(*               args.module_name *)
(*           ) *)
(*           ~encstr:( *)
(*             Printf.sprintf *)
(*               "RequestMessage.enc_opt %s.%s %s.enc" *)
(*               CommandHelper.module_name *)
(*               command *)
(*               args.module_name *)
(*           ) *)
(*           "" *)
(*     | None -> *)
(*         let ty_params = Printf.sprintf "(%s.%s, %s.t option, Presence.opt)" *)
(*             CommandHelper.module_name *)
(*             command *)
(*             Dap_base.EmptyObject.module_name *)
(*         in *)
(*         Render_output.make_msg *)
(*           ~ctor:( *)
(*             Printf.sprintf *)
(*               "let %s req = %s req" *)
(*               (String.uncapitalize_ascii name) name *)
(*           ) *)
(*           ~tystr:( *)
(*             Printf.sprintf *)
(*               "| %s : %s RequestMessage.t -> %s RequestMessage.t t" *)
(*               name ty_params ty_params *)
(*           ) *)
(*           ~ctorstr:(String.uncapitalize_ascii name) *)
(*           ~genstr:( *)
(*             Printf.sprintf *)
(*               "QCheck.Gen.( \ *)
(*                map (fun (seq, arguments) -> RequestMessage.make_opt ~seq ~command:%s.%s ~arguments ()) \ *)
(*                @@ tup2 Gen.gen_int31 %s.gen \ *)
(*                )" *)
(*               CommandHelper.module_name *)
(*               command *)
(*               Dap_base.EmptyObject.module_name *)
(*           ) *)
(*           ~encstr:( *)
(*             Printf.sprintf *)
(*               "RequestMessage.enc_opt %s.%s %s.enc" *)
(*               CommandHelper.module_name *)
(*               command *)
(*               Dap_base.EmptyObject.module_name *)
(*           ) *)
(*           "" *)
(* end *)

(* module RenderResponse : (RenderT with type spec := Sp.Obj_spec.t) = struct *)

(*   type t = Sp.Obj_spec.t *)

(*   let of_spec spec = spec *)

(*   let render (t:t) ~name ~internal_with_sig:_ = *)
(*     let command = CommandHelper.enum_str name ~on:"Response" in *)
(*     match t.fields |> List.find_opt (fun Sp.Field_spec.{safe_name; _} -> safe_name = "body") with *)
(*     | Some body when body.required -> *)
(*         let ty_params = Printf.sprintf "(%s.%s, %s.t, Presence.req)" *)
(*             CommandHelper.module_name *)
(*             command *)
(*             body.module_name *)
(*         in *)
(*         Render_output.make_msg *)
(*           ~ctor:( *)
(*             Printf.sprintf *)
(*               "let %s resp = %s resp" *)
(*               (String.uncapitalize_ascii name) name *)
(*           ) *)
(*           ~tystr:( *)
(*             Printf.sprintf *)
(*               "| %s : %s ResponseMessage.t -> %s ResponseMessage.t t" *)
(*               name ty_params ty_params *)
(*           ) *)
(*           ~ctorstr:(String.uncapitalize_ascii name) *)
(*           ~genstr:( *)
(*             Printf.sprintf *)
(*               "QCheck.Gen.( \ *)
(*                map (fun (seq, request_seq, success, message, body) -> ResponseMessage.make ~seq ~request_seq ~success ~command:%s.%s ?message ~body ()) \ *)
(*                @@ tup5 Gen.gen_int31 Gen.gen_int31 bool Gen.gen_utf8_str_opt %s.gen \ *)
(*                )" *)
(*               CommandHelper.module_name *)
(*               command *)
(*               body.module_name *)
(*           ) *)
(*           ~encstr:( *)
(*             Printf.sprintf *)
(*               "ResponseMessage.enc %s.%s %s.enc" *)
(*               CommandHelper.module_name *)
(*               command *)
(*               body.module_name *)
(*           ) *)
(*           "" *)
(*     | Some body -> *)
(*         let ty_params = Printf.sprintf "(%s.%s, %s.t option, Presence.opt)" *)
(*             CommandHelper.module_name *)
(*             command *)
(*             body.module_name *)
(*         in *)
(*         Render_output.make_msg *)
(*           ~ctor:( *)
(*             Printf.sprintf *)
(*               "let %s resp = %s resp" *)
(*               (String.uncapitalize_ascii name) name *)
(*           ) *)
(*           ~tystr:( *)
(*             Printf.sprintf *)
(*               "| %s : %s ResponseMessage.t -> %s ResponseMessage.t t" *)
(*               name ty_params ty_params *)
(*           ) *)
(*           ~ctorstr:(String.uncapitalize_ascii name) *)
(*           ~genstr:( *)
(*             Printf.sprintf *)
(*               "QCheck.Gen.( \ *)
(*                map (fun (seq, request_seq, success, message, body) -> ResponseMessage.make_opt ~seq ~request_seq ~success ~command:%s.%s ?message ~body ()) \ *)
(*                @@ tup5 Gen.gen_int31 Gen.gen_int31 bool Gen.gen_utf8_str_opt %s.gen \ *)
(*                )" *)
(*               CommandHelper.module_name *)
(*               command *)
(*               body.module_name *)
(*           ) *)
(*           ~encstr:( *)
(*             Printf.sprintf *)
(*               "ResponseMessage.enc_opt %s.%s %s.enc" *)
(*               CommandHelper.module_name *)
(*               command *)
(*               body.module_name *)
(*           ) *)
(*           "" *)
(*     | None -> *)
(*         let ty_params = Printf.sprintf "(%s.%s, %s.t option, Presence.opt)" *)
(*             CommandHelper.module_name *)
(*             command *)
(*             Dap_base.EmptyObject.module_name *)
(*         in *)
(*         Render_output.make_msg *)
(*           ~ctor:( *)
(*             Printf.sprintf *)
(*               "let %s resp = %s resp" *)
(*               (String.uncapitalize_ascii name) name *)
(*           ) *)
(*           ~tystr:( *)
(*             Printf.sprintf *)
(*               "| %s : %s ResponseMessage.t -> %s ResponseMessage.t t" *)
(*               name ty_params ty_params *)
(*           ) *)
(*           ~ctorstr:(String.uncapitalize_ascii name) *)
(*           ~genstr:( *)
(*             Printf.sprintf *)
(*               "QCheck.Gen.( \ *)
(*                map (fun (seq, request_seq, success, message, body) -> ResponseMessage.make_opt ~seq ~request_seq ~success ~command:%s.%s ?message ~body ()) \ *)
(*                @@ tup5 Gen.gen_int31 Gen.gen_int31 bool Gen.gen_utf8_str_opt %s.gen \ *)
(*                )" *)
(*               CommandHelper.module_name *)
(*               command *)
(*               Dap_base.EmptyObject.module_name *)
(*           ) *)
(*           ~encstr:( *)
(*             Printf.sprintf *)
(*               "ResponseMessage.enc_opt %s.%s %s.enc" *)
(*               CommandHelper.module_name *)
(*               command *)
(*               Dap_base.EmptyObject.module_name *)
(*           ) *)
(*           "" *)
(* end *)

(* module RenderEvent : (RenderT with type spec := Sp.Obj_spec.t) = struct *)

(*   type t = Sp.Obj_spec.t *)

(*   let of_spec spec = spec *)

(*   let render (t:t) ~name ~internal_with_sig:_ = *)
(*     let event = EventHelper.enum_str name in *)
(*     match t.fields |> List.find_opt (fun Sp.Field_spec.{safe_name; _} -> safe_name = "body") with *)
(*     | Some body when body.required -> *)
(*         let ty_params = Printf.sprintf "(%s.%s, %s.t, Presence.req)" *)
(*             EventHelper.module_name *)
(*             event *)
(*             body.module_name *)
(*         in *)
(*         Render_output.make_msg *)
(*           ~ctor:( *)
(*             Printf.sprintf *)
(*               "let %s ev = %s ev" *)
(*               (String.uncapitalize_ascii name) name *)
(*           ) *)
(*           ~tystr:( *)
(*             Printf.sprintf *)
(*               "| %s : %s EventMessage.t -> %s EventMessage.t t" *)
(*               name ty_params ty_params *)
(*           ) *)
(*           ~ctorstr:(String.uncapitalize_ascii name) *)
(*           ~genstr:( *)
(*             Printf.sprintf *)
(*               "QCheck.Gen.( \ *)
(*                map (fun (seq, body) -> EventMessage.make ~seq ~event:%s.%s ~body ()) \ *)
(*                @@ tup2 Gen.gen_int31 %s.gen \ *)
(*                )" *)
(*               EventHelper.module_name *)
(*               event *)
(*               body.module_name *)
(*           ) *)
(*           ~encstr:( *)
(*             Printf.sprintf *)
(*               "EventMessage.enc %s.%s %s.enc" *)
(*               EventHelper.module_name *)
(*               event *)
(*               body.module_name *)
(*           ) *)
(*           "" *)

(*     | Some body -> *)
(*         let ty_params = Printf.sprintf "(%s.%s, %s.t option, Presence.opt)" *)
(*             EventHelper.module_name *)
(*             event *)
(*             body.module_name *)
(*         in *)
(*         Render_output.make_msg *)
(*           ~ctor:( *)
(*             Printf.sprintf *)
(*               "let %s ev = %s ev" *)
(*               (String.uncapitalize_ascii name) name *)
(*           ) *)
(*           ~tystr:( *)
(*             Printf.sprintf *)
(*               "| %s : %s EventMessage.t -> %s EventMessage.t t" *)
(*               name ty_params ty_params *)
(*           ) *)
(*           ~ctorstr:(String.uncapitalize_ascii name) *)
(*           ~genstr:( *)
(*             Printf.sprintf *)
(*               "QCheck.Gen.( \ *)
(*                map (fun (seq, body) -> EventMessage.make_opt ~seq ~event:%s.%s ~body ()) \ *)
(*                @@ tup2 Gen.gen_int31 %s.gen \ *)
(*                )" *)
(*               EventHelper.module_name *)
(*               event *)
(*               body.module_name *)
(*           ) *)
(*           ~encstr:( *)
(*             Printf.sprintf *)
(*               "EventMessage.enc_opt %s.%s %s.enc" *)
(*               EventHelper.module_name *)
(*               event *)
(*               body.module_name *)
(*           ) *)
(*           "" *)
(*     | None -> *)
(*         let ty_params = Printf.sprintf "(%s.%s, %s.t option, Presence.opt)" *)
(*             EventHelper.module_name *)
(*             event *)
(*             Dap_base.EmptyObject.module_name *)
(*         in *)
(*         Render_output.make_msg *)
(*           ~ctor:( *)
(*             Printf.sprintf *)
(*               "let %s ev = %s ev" *)
(*               (String.uncapitalize_ascii name) name *)
(*           ) *)
(*           ~tystr:( *)
(*             Printf.sprintf *)
(*               "| %s : %s EventMessage.t -> %s EventMessage.t t" *)
(*               name ty_params ty_params *)
(*           ) *)
(*           ~ctorstr:(String.uncapitalize_ascii name) *)
(*           ~genstr:( *)
(*             Printf.sprintf *)
(*               "QCheck.Gen.( \ *)
(*                map (fun (seq, body) -> EventMessage.make_opt ~seq ~event:%s.%s ~body ()) \ *)
(*                @@ tup2 Gen.gen_int31 %s.gen \ *)
(*                )" *)
(*               EventHelper.module_name *)
(*               event *)
(*               Dap_base.EmptyObject.module_name *)
(*           ) *)
(*           ~encstr:( *)
(*             Printf.sprintf *)
(*               "EventMessage.enc_opt %s.%s %s.enc" *)
(*               EventHelper.module_name *)
(*               event *)
(*               Dap_base.EmptyObject.module_name *)
(*           ) *)
(*           "" *)
(* end *)

(* type ftype = | ML | MLI *)

(* type what = *)
(*   | Events of ftype | Commands of ftype | Messages *)

(* let render (dfs:Dfs.t) = let open Render_output in function *)
(*   | Messages -> *)
(*     let modstrs = ref [] in *)
(*     let reqstrs = ref [] in *)
(*     let reqctors = ref [] in *)
(*     let reqgens = ref [] in *)
(*     let respstrs = ref [] in *)
(*     let respctors = ref [] in *)
(*     let respgens = ref [] in *)
(*     let eventstrs = ref [] in *)
(*     let eventctors = ref [] in *)
(*     let eventgens = ref [] in *)
(*     let _ = *)
(*       Dfs.ordering dfs *)
(*       |> List.iter (fun name -> *)
(*           match (Hashtbl.find_opt dfs.finished name) with *)
(*           | Some (Sp.Request o) -> *)
(*             let {modstr; tystr; ctor; ctorstr; genstr; encstr} = match RenderRequest.(of_spec o |> render ~name ~internal_with_sig:true) with `Message msg -> msg | _ -> assert false in *)
(*             modstrs := modstr :: !modstrs; reqstrs := tystr :: !reqstrs; reqctors := ctor :: !reqctors; reqgens := (ctorstr, genstr, encstr) :: !reqgens *)
(*           | Some (Sp.Response o) -> *)
(*             let {modstr; tystr; ctor; ctorstr; genstr; encstr} = match RenderResponse.(of_spec o |> render ~name ~internal_with_sig:true) with `Message msg -> msg | _ -> assert false in *)
(*             modstrs := modstr :: !modstrs; respstrs := tystr :: !respstrs; respctors := ctor :: !respctors; respgens := (ctorstr, genstr, encstr) :: !respgens *)
(*           | Some (Sp.Event o) -> *)
(*             let {modstr; tystr; ctor; ctorstr; genstr; encstr} = match RenderEvent.(of_spec o |> render ~name ~internal_with_sig:true) with `Message msg -> msg | _ -> assert false in *)
(*             modstrs := modstr :: !modstrs; eventstrs := tystr :: !eventstrs; eventctors := ctor :: !eventctors; eventgens := (ctorstr, genstr, encstr) :: !eventgens *)
(*           | Some (Sp.Object o) when Sp.Obj_spec.is_big o -> *)
(*             let {modstr; _} = match RenderLargeObject.(of_spec o |> render ~name ~internal_with_sig:true) with `Message msg -> msg | _ -> assert false in *)
(*             modstrs := modstr :: !modstrs *)
(*           | Some (Sp.Object o) when Sp.Obj_spec.is_empty o -> *)
(*             let {modstr; _} = match RenderEmptyObject.(of_spec () |> render ~name ~internal_with_sig:true) with `Message msg -> msg | _ -> assert false in *)
(*             modstrs := modstr :: !modstrs *)
(*           | Some (Sp.Object o) -> *)
(*             let {modstr; _} = match RenderObject.(of_spec o |> render ~name ~internal_with_sig:true) with `Message msg -> msg | _ -> assert false in *)
(*             modstrs := modstr :: !modstrs *)
(*           | Some (Sp.Enum e) -> *)
(*             let {modstr; _} = match RenderEnum.(of_spec e |> render ~name ~internal_with_sig:true) with `Message msg -> msg | _ -> assert false in *)
(*             modstrs := modstr :: !modstrs *)
(*           | Some _ -> assert false *)
(*           | None -> Logs.warn (fun m -> m "couldn't find '%s', ignoring" name) *)
(*         ) *)
(*     in *)
(*     let smods = String.concat "\n\n" (!modstrs |> List.rev) in *)
(*     let sreqs = String.concat "\n" (!reqstrs |> List.rev) in *)
(*     let sreqctors = String.concat "\n" (!reqctors |> List.rev) in *)
(*     let sreqgens = String.concat "\n" ( *)
(*         !reqgens *)
(*         |> List.rev *)
(*         |> List.map (fun (ctor, gen, enc) -> Printf.sprintf "let gen_%s = (%s, %s, %s)" ctor ctor gen enc) *)
(*       ) *)
(*     in *)
(*     let sresps = String.concat "\n" (!respstrs |> List.rev) in *)
(*     let srespctors = String.concat "\n" (!respctors |> List.rev) in *)
(*     let srespgens = String.concat "\n" ( *)
(*         !respgens *)
(*         |> List.rev *)
(*         |> List.map (fun (ctor, gen, enc) -> Printf.sprintf "let gen_%s = (%s, %s, %s)" ctor ctor gen enc) *)
(*       ) *)
(*     in *)
(*     let sevents = String.concat "\n" (!eventstrs |> List.rev) in *)
(*     let seventctors = String.concat "\n" (!eventctors |> List.rev) in *)
(*     let seventgens = String.concat "\n" ( *)
(*         !eventgens *)
(*         |> List.rev *)
(*         |> List.map (fun (ctor, gen, enc) -> Printf.sprintf "let gen_%s = (%s, %s, %s)" ctor ctor gen enc) *)
(*       ) *)
(*     in *)
(*     Printf.sprintf *)
(*       "(\* NOTE this file was autogenerated - do not modify by hand *\)\n\n \ *)
(*        module RequestMessage = Dap_request_message\n \ *)
(*        module ResponseMessage = Dap_response_message\n \ *)
(*        module EventMessage = Dap_event_message\n\n \ *)
(*        module Data = struct \n\n \ *)
(*        include Dap_base\n\n \ *)
(*        (\* supporting data modules *\) %s\n\n \ *)
(*        end \n\n \ *)
(*        module Request  = struct \n\n \ *)
(*        open Data \n\n \ *)
(*        type _ t = \n *)
(*        | Fmap : ('a -> 'b) -> ('a -> 'b) t \n *)
(*        (\* request GADT items *\) %s\n\n \ *)
(*        (\* request constructors *\) %s\n\n \ *)
(*        (\* request qcheck *\) %s\n\n \ *)
(*        end \n\n \ *)
(*        module Response = struct \n\n \ *)
(*        open Data \n\n \ *)
(*        type _ t = \n *)
(*        | Fmap : ('a -> 'b) -> ('a -> 'b) t \n *)
(*        (\* response GADT items *\) %s\n\n \ *)
(*        (\* response constructors *\) %s\n\n \ *)
(*        (\* response qcheck *\) %s\n\n \ *)
(*        end \n\n \ *)
(*        module Event = struct \n\n \ *)
(*        open Data \n\n \ *)
(*        type _ t = \n *)
(*        | Fmap : ('a -> 'b) -> ('a -> 'b) t \n *)
(*        (\* event GADT items *\) %s\n\n *)
(*        (\* event constructors *\) %s\n\n \ *)
(*        (\* event qcheck *\) %s\n\n \ *)
(*        end" *)
(*       smods sreqs sreqctors sreqgens sresps srespctors srespgens sevents seventctors seventgens *)
(*   | Commands ML -> *)
(*     let {ml; _} = match RenderEnumWithPhantoms.(of_spec dfs.command_enum |> render ~name:CommandHelper.module_name ~internal_with_sig:false) with `MlMli mlmli -> mlmli | _ -> assert false in *)
(*     ml *)
(*   | Commands MLI -> *)
(*     let {mli; _} = match RenderEnumWithPhantoms.(of_spec dfs.command_enum |> render ~name:CommandHelper.module_name ~internal_with_sig:false) with `MlMli mlmli -> mlmli | _ -> assert false in *)
(*     mli *)
(*   | Events ML -> *)
(*     let {ml; _} = match RenderEnumWithPhantoms.(of_spec dfs.event_enum |> render ~name:EventHelper.module_name ~internal_with_sig:false) with `MlMli mlmli -> mlmli | _ -> assert false in *)
(*     ml *)
(*   | Events MLI -> *)
(*     let {mli; _} = match RenderEnumWithPhantoms.(of_spec dfs.event_enum |> render ~name:EventHelper.module_name ~internal_with_sig:false) with `MlMli mlmli -> mlmli | _ -> assert false in *)
(*     mli *)
