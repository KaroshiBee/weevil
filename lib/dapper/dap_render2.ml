
module Sp = Dap_specs
module CommandHelper = Dap_dfs.CommandHelper
module EventHelper = Dap_dfs.EventHelper
module Dfs = Dap_dfs.Dfs

(* TODO want to be able to easily add new derivings *)

(* NOTE these @@ are awkard when used directly in format string *)
module Consts = struct
  let atat = "@@"
  let deriving_str = Fmt.str "%sderiving" atat
end


module Enum = struct

  type element = {
    element_name : string
  ; element_dirty_name : string
  ; element_index : int
  } [@@deriving show, eq]

  and t = {
    enum_name : string
  ; enum_type : [ `Open | `Closed ] (* whether it allows for `| Other of string` *)
  ; enum_elements : element list
  }

  let ordered_elements els =
    els
    |> List.sort (fun x y -> compare x.element_index y.element_index)

  let of_enum_spec Sp.Enum_spec.{safe_name; enums; suggested; _} =
    let enum_type = if suggested then `Open else `Closed in
    let enum_elements =
      List.mapi (fun element_index (e:Sp.Enum_spec.enum_val) -> {
            element_name=e.safe_name;
            element_dirty_name=e.dirty_name;
            element_index
          }) enums
    in
    {enum_name=safe_name; enum_type; enum_elements}

  let test_data ~enum_type () =
    let enum_name = "Stopped_event_enum" in
    let enum_elements = [
      {element_name="Breakpoint";
       element_dirty_name="breakpoint";
       element_index=1};
      {element_name="Step";
       element_dirty_name="step";
       element_index=0};
      {element_name="Exception";
       element_dirty_name="exception";
       element_index=2};
    ] in
    {enum_name; enum_type; enum_elements}

end



(* NOTE dont need a sig for the enum types, inferred one is fine *)
module Stanza_enum_t_struct = struct

  let pp =
    Fmt.of_to_string (function Enum.{enum_elements; enum_type; _} ->
        let pp_element =
          Fmt.of_to_string (function Enum.{element_name; _} ->
              Fmt.str "| %s" element_name
            )
        in
        let pp_elements =
          Fmt.list ~sep:(Fmt.any "\n") pp_element
        in
        let elements = Enum.ordered_elements enum_elements in
        let deriving = Fmt.str "[%s irmin, qcheck]" Consts.deriving_str in
        let last = function
          | `Open -> ["| Other of string"; deriving]
          | `Closed -> [deriving]
        in
        String.concat "\n" @@ List.concat [
          [Fmt.str "type t =\n%a" pp_elements elements];
          last enum_type;
          [Fmt.str "let equal = Irmin.Type.(unstage (equal t))";
           Fmt.str "let merge = Irmin.Merge.idempotent t"];
        ]
      )

  let%expect_test "Check Stanza_enum_t_struct Open" =
    let grp = Enum.test_data ~enum_type:`Open () in
    print_endline @@ Format.asprintf "%a" pp grp;
    [%expect {|
      type t =
      | Step
      | Breakpoint
      | Exception
      | Other of string
      [@@deriving irmin, qcheck]
      let equal = Irmin.Type.(unstage (equal t))
      let merge = Irmin.Merge.idempotent t |}]

  let%expect_test "Check Stanza_enum_t_struct Closed" =
    let grp = Enum.test_data ~enum_type:`Closed () in
    print_endline @@ Format.asprintf "%a" pp grp;
    [%expect {|
      type t =
      | Step
      | Breakpoint
      | Exception
      [@@deriving irmin, qcheck]
      let equal = Irmin.Type.(unstage (equal t))
      let merge = Irmin.Merge.idempotent t |}]

end

module Stanza_enum_enc_struct = struct

  let pp =
    Fmt.of_to_string (function Enum.{enum_name; enum_elements; enum_type; _} ->
        let pp_name_to_dirty =
          Fmt.of_to_string (function Enum.{element_name; element_dirty_name; _} ->
              Fmt.str "| %s -> \"%s\"" element_name element_dirty_name
            )
        in
        let last1 = function
          | `Open -> "| Other s -> s"
          | `Closed -> ""
        in
        let pp_dirty_to_name =
          Fmt.of_to_string (function Enum.{element_name; element_dirty_name; _} ->
              Fmt.str "| \"%s\" -> Ok %s" element_dirty_name element_name
            )
        in
        let last2 = function
          | `Open -> "| _ as s -> Ok (Other s)"
          | `Closed -> Fmt.str "| _ -> Error \"%s\"" enum_name
        in
        let pp_elements pp_ =
          Fmt.list ~sep:(Fmt.any "\n") pp_
        in

        let elements = Enum.ordered_elements enum_elements in
        String.concat "\n" [
          Fmt.str "let enc =";
          "let open Data_encoding in ";
          "conv_with_guard";
          Fmt.str "(function\n%a\n%s)" (pp_elements pp_name_to_dirty) elements (last1 enum_type);
          Fmt.str "(function\n%a\n%s)" (pp_elements pp_dirty_to_name) elements (last2 enum_type);
          "string"
        ]
      )

  let%expect_test "Check Stanza_enum_enc_struct Open" =
    let grp = Enum.test_data ~enum_type:`Open () in
    print_endline @@ Format.asprintf "%a" pp grp;
    [%expect {|
      let enc =
      let open Data_encoding in
      conv_with_guard
      (function
      | Step -> "step"
      | Breakpoint -> "breakpoint"
      | Exception -> "exception"
      | Other s -> s)
      (function
      | "step" -> Ok Step
      | "breakpoint" -> Ok Breakpoint
      | "exception" -> Ok Exception
      | _ as s -> Ok (Other s))
      string |}]

  let%expect_test "Check Stanza_enum_enc_struct Closed" =
    let grp = Enum.test_data ~enum_type:`Closed () in
    print_endline @@ Format.asprintf "%a" pp grp;
    [%expect {|
      let enc =
      let open Data_encoding in
      conv_with_guard
      (function
      | Step -> "step"
      | Breakpoint -> "breakpoint"
      | Exception -> "exception"
      )
      (function
      | "step" -> Ok Step
      | "breakpoint" -> Ok Breakpoint
      | "exception" -> Ok Exception
      | _ -> Error "Stopped_event_enum")
      string |}]

end


module Printer_enum = struct

  let pp_struct =
    Fmt.of_to_string (function e ->
        String.concat "\n\n" [
          Fmt.str "%a" Stanza_enum_t_struct.pp e;
          Fmt.str "%a" Stanza_enum_enc_struct.pp e;
        ]
      )

  let pp =
    Fmt.of_to_string (function e ->
        Fmt.str "module %s = struct\n%a\nend"
          e.Enum.enum_name pp_struct e
      )

  let%expect_test "Check Printer_enum open" =
    let grp = Enum.test_data ~enum_type:`Open () in
    print_endline @@ Format.asprintf "%a" pp grp;
    [%expect {|
      module Stopped_event_enum = struct
      type t =
      | Step
      | Breakpoint
      | Exception
      | Other of string
      [@@deriving irmin, qcheck]
      let equal = Irmin.Type.(unstage (equal t))
      let merge = Irmin.Merge.idempotent t

      let enc =
      let open Data_encoding in
      conv_with_guard
      (function
      | Step -> "step"
      | Breakpoint -> "breakpoint"
      | Exception -> "exception"
      | Other s -> s)
      (function
      | "step" -> Ok Step
      | "breakpoint" -> Ok Breakpoint
      | "exception" -> Ok Exception
      | _ as s -> Ok (Other s))
      string
      end |}]

  let%expect_test "Check Printer_enum closed" =
    let grp = Enum.test_data ~enum_type:`Closed () in
    print_endline @@ Format.asprintf "%a" pp grp;
    [%expect {|
      module Stopped_event_enum = struct
      type t =
      | Step
      | Breakpoint
      | Exception
      [@@deriving irmin, qcheck]
      let equal = Irmin.Type.(unstage (equal t))
      let merge = Irmin.Merge.idempotent t

      let enc =
      let open Data_encoding in
      conv_with_guard
      (function
      | Step -> "step"
      | Breakpoint -> "breakpoint"
      | Exception -> "exception"
      )
      (function
      | "step" -> Ok Step
      | "breakpoint" -> Ok Breakpoint
      | "exception" -> Ok Exception
      | _ -> Error "Stopped_event_enum")
      string
      end |}]

end


module Obj = struct

  type field = {
    field_name : string
  ; field_dirty_name : string
  ; field_type : [
      `Builtin of builtin
    | `User_defined of t
    ]
  ; field_presence : [`Opt | `Req ]
  ; field_container : container option (* we only deal with containers of one type e.g. 'a list *)
  ; field_gen_container : gen_container option (* individual generator decorators for fields e.g. [@gen Gen.gen_utf8_str_opt] *)
  ; field_index : int
  } [@@deriving show, eq]

  and t = {
    (* what the struct would be called ie Thing *)
    object_name : string
  (* what 't' would be called i.e. for Thing.t *)
  ; object_t : string
  (* what 'enc' would be called, cyclic is special case (needs mu encoder),
     raw is just as is, qualified needs to be combined with object_name *)
  ; object_enc : [ `Cyclic | `Raw of string | `Qualified of string ]
  ; object_fields : field list
  } [@@deriving show, eq ]

  and builtin = {
    builtin_type : string
  ; builtin_enc : string
  } [@@deriving show, eq ]

  and container = {
    container_type : string
  ; container_enc : string
  } [@@deriving show, eq ]

  and gen_container = {
    gen_name : string
  } [@@deriving show, eq ]

  let of_field_spec field_index Sp.Field_spec.{
      safe_name=field_name;
      dirty_name=field_dirty_name;
      module_name;
      type_;
      enc_;
      required;
      seq;
      kind;
      _} =
    let field_type = match kind with
      | `Builtin -> `Builtin {builtin_type=type_;
                              builtin_enc=enc_}
      | `Jsonish -> `User_defined {object_name="Irmin.Contents.Json_value";
                                   object_t="t";
                                   object_enc=`Raw "json";
                                   object_fields=[]}
      | `Other   -> `User_defined {object_name=module_name;
                                   object_t="t";
                                   object_enc=`Raw enc_;
                                   object_fields=[]}
    in
    let field_presence = if required then `Req else `Opt in
    let field_container = if seq then Some {container_type="list"; container_enc="enc"} else None in
    let field_gen_container = match enc_ with
      | "int31" -> Some {gen_name="gen_int31"}
      | "string" -> Some {gen_name="gen_utf8_str"}
      | "json" -> Some {gen_name="gen_json"}
      | _ -> None
    in
    {field_name; field_dirty_name; field_type; field_presence; field_container; field_gen_container; field_index}

  let of_obj_spec Sp.Obj_spec.{safe_name; fields; is_cyclic; _} =
    let object_name = safe_name in
    let object_t = "t" in
    let object_enc = if is_cyclic then `Cyclic else `Raw "enc" in
    let object_fields = List.mapi of_field_spec fields
    in
    {object_name; object_t; object_enc; object_fields}


  let ordered_fields fields =
    fields
    |> List.sort (fun x y -> compare x.field_index y.field_index)

  (* NOTE only go one level deep *)
  let has_cycle =
    function {object_fields; _} ->
      List.exists
        (function
          | {field_type=`User_defined {object_enc=`Cyclic; _}; _} -> true
          | _ -> false
        ) object_fields

  (* NOTE only go one level deep *)
  let cyclic_fields =
    function {object_fields; _} ->
      List.filter
        (function
          | {field_type=`User_defined {object_enc=`Cyclic; _}; _} -> true
          | _ -> false
        ) object_fields

  let test_data () =
    let object_name = "Thing" in
    let object_t = "t" in
    let object_enc = `Qualified "enc" in
    let object_fields = [
      { field_name="variables";
        field_dirty_name="_variables_";
        field_type=`User_defined {object_name="Irmin.Contents.Json_value";
                                  object_t="t";
                                  object_enc = `Raw "Data_encoding.json";
                                  object_fields=[]};
        field_presence=`Opt;
        field_container=None;
        field_gen_container=Some {gen_name="gen_json"};
        field_index=2 };
      { field_name="format";
        field_dirty_name="format_";
        field_type=`Builtin {builtin_type="string";
                             builtin_enc="string"};
        field_presence=`Req;
        field_container=Some {container_type="list";
                              container_enc="list"};
        field_gen_container=Some {gen_name="gen_utf8_str"};
        field_index=1 };
      { field_name="sendTelemetry";
        field_dirty_name="sendTelemetry_";
        field_type=`Builtin {builtin_type="bool";
                             builtin_enc="bool"};
        field_presence=`Opt;
        field_container=None;
        field_gen_container=None;
        field_index=3 };
      { field_name="things";
        field_dirty_name="things in a container";
        field_type=`User_defined {object_name="Thing";
                                  object_t="t";
                                  object_enc = `Cyclic;
                                  object_fields=[]};
        field_presence=`Opt;
        field_container=Some {container_type="tree";
                              container_enc="tree_enc"};
        field_gen_container=None;
        field_index=4 };
      { field_name="stuff";
        field_dirty_name="stuff";
        field_type=`User_defined {object_name="Stopped_event_enum";
                                  object_t="t";
                                  object_enc=`Qualified "enc";
                                  object_fields=[]};
        field_presence=`Opt;
        field_container=None;
        field_gen_container=None;
        field_index=5 };
    ]
    in
    {object_name; object_t; object_enc; object_fields}
end

module type PP_struct = sig
  val pp : Format.formatter -> 'a -> unit
end

module Stanza_t_sig = struct

  (* TODO compose in the deriving irmin *)
  let pp =
    Fmt.of_to_string (function Obj.{object_name=_; _} ->
        let ss = [
          Fmt.str "type t [%s irmin]" Consts.deriving_str;
          Fmt.str "val equal : t -> t -> bool";
          Fmt.str "val merge : t Irmin.Merge.t";
          Fmt.str "val gen : t QCheck.Gen.t";
          Fmt.str "val arb : t QCheck.arbitrary";
        ] in
        String.concat "\n\n" ss
      )

  let%expect_test "Check Stanza_t_sig" =
    let grp = Obj.test_data () in
    print_endline @@ Format.asprintf "%a" pp grp;
    [%expect {|
      type t [@@deriving irmin]

      val equal : t -> t -> bool

      val merge : t Irmin.Merge.t

      val gen : t QCheck.Gen.t

      val arb : t QCheck.arbitrary |}]

end

module Stanza_t_struct = struct

  let pp_field ~cyclic_field =
    Fmt.of_to_string (function Obj.{field_name; field_type; field_presence; field_container; field_gen_container; _} as f ->
        let presence = match field_presence with | `Opt -> "option" | `Req -> "" in
        let container = match field_container with None -> "" | Some {container_type; _} -> container_type in
        let gen_name = match field_gen_container with
          | None -> ""
          | Some {gen_name; _} -> match (field_presence, field_container) with
            | `Opt, None -> Fmt.str "[@gen Gen.%s_opt]" gen_name
            | `Opt, Some {container_type; _} -> Fmt.str "[@gen Gen.%s_%s_opt]" gen_name container_type
            | `Req, None -> Fmt.str "[@gen Gen.%s]" gen_name
            | `Req, Some {container_type; _} -> Fmt.str "[@gen Gen.%s_%s]" gen_name container_type
        in
        match field_type with
        | `Builtin {builtin_type; _} ->
          Fmt.str "%s : (%s %s %s %s)" field_name builtin_type container presence gen_name
        | `User_defined {object_t; _} when cyclic_field = Some f ->
          Fmt.str "%s : (%s %s %s %s)" field_name object_t container presence gen_name
        | `User_defined {object_name; object_t; _} ->
          Fmt.str "%s : (%s.%s %s %s %s)" field_name object_name object_t container presence gen_name
      )

  let pp =
    Fmt.of_to_string (function Obj.{object_fields; object_t; _} as o ->
        let fields = Obj.ordered_fields object_fields in
        let cyclic = Obj.has_cycle o in
        let cyclic_field =
          let xs = Obj.cyclic_fields o in
          List.nth_opt xs 0
        in
        let pp_fields = Fmt.list ~sep:(Fmt.any ";\n") (pp_field ~cyclic_field) in
        let ss = [
          Fmt.str "type t = {\n%a\n}\n[%s irmin%s]"
            pp_fields fields
            Consts.deriving_str
            (if cyclic then "" else ", qcheck");
          Fmt.str "let equal = Irmin.Type.(unstage (equal %s))" object_t;
          Fmt.str "let merge = Irmin.Merge.idempotent %s" object_t;
        ] in
        String.concat "\n\n" ss
      )

  let%expect_test "Check Stanza_struct" =
    let grp = Obj.test_data () in
    print_endline @@ Format.asprintf "%a" pp grp;
    [%expect {|
      type t = {
      format : (string list  [@gen Gen.gen_utf8_str_list]);
      variables : (Irmin.Contents.Json_value.t  option [@gen Gen.gen_json_opt]);
      sendTelemetry : (bool  option );
      things : (t tree option );
      stuff : (Stopped_event_enum.t  option )
      }
      [@@deriving irmin]

      let equal = Irmin.Type.(unstage (equal t))

      let merge = Irmin.Merge.idempotent t |}]

end

module Stanza_gen_struct = struct

  let pp_field_name = Fmt.of_to_string (function Obj.{field_name; _} -> field_name)

  let pp_names ~tok = Fmt.list ~sep:(Fmt.any tok) pp_field_name

  let pp_gen ~cyclic_field =
    let gen_name_str gen_name container =
      let s = match container with None -> gen_name | Some Obj.{container_type; _} -> Fmt.str "%s_%s" gen_name container_type in
      function
      | `Opt -> Fmt.str "Gen.%s_opt" s
      | `Req -> Fmt.str "Gen.%s" s
    in
    let name_str name container =
      let s = match container with None -> name | Some Obj.{container_type; _} -> Fmt.str "(%s %s)" container_type name in
      function `Opt -> Fmt.str "(option %s)" s | `Req -> Fmt.str "(%s)" s
    in
    Fmt.of_to_string (function
        | f when f = cyclic_field -> "t"
        | Obj.{field_gen_container=Some {gen_name; _}; field_container; field_presence; _} ->
          gen_name_str gen_name field_container field_presence
        | Obj.{field_gen_container=None; field_container; field_presence; field_type=`Builtin {builtin_type; _}; _} ->
          name_str builtin_type field_container field_presence
        | Obj.{field_gen_container=None; field_container; field_presence; field_type=`User_defined {object_name; _}; _} ->
          name_str (object_name^".gen") field_container field_presence
      )

  let pp_gens ~cyclic_field = Fmt.list ~sep:(Fmt.any "\n") (pp_gen ~cyclic_field)

  let pp =
    Fmt.of_to_string (function
        | Obj.{object_fields; _} as st when Obj.has_cycle st ->
          let fields = Obj.ordered_fields object_fields in
          let cyclic_fields = Obj.cyclic_fields st in
          assert (1 = List.length cyclic_fields);
          let cyclic_field = List.nth cyclic_fields 0 in
          let ss = [
            Fmt.str "let gen = QCheck.Gen.(sized %s fix (fun self n -> " Consts.atat;
            Fmt.str "let basecase = oneofl [None; Some []] in ";
            Fmt.str "let _gen_t = ";
            Fmt.str "fun t -> ";
            Fmt.str "let gg = tup%d\n%a in" (List.length fields) (pp_gens ~cyclic_field) fields;
            Fmt.str "map (fun ( %a ) -> { %a }) gg" (pp_names ~tok:",") fields (pp_names ~tok:";") fields;
            Fmt.str "in match n with";
            Fmt.str "| 0 -> _gen_t basecase";
            Fmt.str "| n -> frequency";
            Fmt.str "[(1, _gen_t basecase);\n(1, let t = map (fun {%a; _} -> %a) %s self (n / 2) in _gen_t t)]"
              pp_field_name cyclic_field
              pp_field_name cyclic_field
              Consts.atat;
            Fmt.str "))\n";
            Fmt.str "let arb = QCheck.make gen";
          ]
          in
          String.concat "\n" ss
        | _st -> ""
      )

  let%expect_test "Check Stanza_gen_struct" =
    let grp = Obj.test_data () in
    print_endline @@ Format.asprintf "%a" pp grp;
    [%expect {|
      let gen = QCheck.Gen.(sized @@ fix (fun self n ->
      let basecase = oneofl [None; Some []] in
      let _gen_t =
      fun t ->
      let gg = tup5
      Gen.gen_utf8_str_list
      Gen.gen_json_opt
      (option bool)
      t
      (option Stopped_event_enum.gen) in
      map (fun ( format,variables,sendTelemetry,things,stuff ) -> { format;variables;sendTelemetry;things;stuff }) gg
      in match n with
      | 0 -> _gen_t basecase
      | n -> frequency
      [(1, _gen_t basecase);
      (1, let t = map (fun {things; _} -> things) @@ self (n / 2) in _gen_t t)]
      ))

      let arb = QCheck.make gen |}]

end

module Stanza_make_sig = struct

  let pp_field ~cyclic_field =
    Fmt.of_to_string (function Obj.{field_name; field_type; field_presence; field_container; _} as f ->
        let presence = match field_presence with | `Opt -> "?" | `Req -> "" in
        let container = match field_container with None -> "" | Some {container_type; _} -> container_type in
        match field_type with
        | `Builtin {builtin_type; _} ->
          Fmt.str "%s%s : %s %s" presence field_name builtin_type container
        | `User_defined {object_t; _} when cyclic_field = Some f ->
          Fmt.str "%s%s : %s %s" presence field_name object_t container
        | `User_defined {object_t; object_name; _} ->
          Fmt.str "%s%s : %s.%s %s" presence field_name object_name object_t container
      )

  let pp =
    Fmt.of_to_string (function Obj.{object_fields; _} as o ->
        let fields = Obj.ordered_fields object_fields in
        let cyclic_field =
          let xs = Obj.cyclic_fields o in
          List.nth_opt xs 0
        in
        let pp_fields = Fmt.list ~sep:(Fmt.any " -> \n") (pp_field ~cyclic_field) in
        Fmt.str "val make : \n%a -> \nunit -> \nt" pp_fields fields
      )

  let%expect_test "Check Stanza_make_sig" =
    let grp = Obj.test_data () in
    print_endline @@ Format.asprintf "%a" pp grp;
    [%expect {|
      val make :
      format : string list ->
      ?variables : Irmin.Contents.Json_value.t  ->
      ?sendTelemetry : bool  ->
      ?things : t tree ->
      ?stuff : Stopped_event_enum.t  ->
      unit ->
      t |}]

end

module Stanza_make_struct = struct

  let pp_field_upper =
    Fmt.of_to_string (function Obj.{field_name; field_presence; _} ->
        let presence = match field_presence with | `Opt -> "?" | `Req -> "~" in
        Fmt.str "%s%s" presence field_name
      )

  let pp_field_lower =
    Fmt.of_to_string (function Obj.{field_name; _} ->
        Fmt.str "%s" field_name
      )

  let pp =
    Fmt.of_to_string (function Obj.{object_fields; _} ->
        let all_fields = Obj.ordered_fields object_fields in
        let pp_upper = Fmt.list ~sep:(Fmt.any " ") pp_field_upper in
        let pp_lower = Fmt.list ~sep:(Fmt.any "; ") pp_field_lower in
        Fmt.str "let make %a () =\n{%a}\n" pp_upper all_fields pp_lower all_fields
      )

  let%expect_test "Check Stanza_make_struct" =
    let grp = Obj.test_data () in
    print_endline @@ Format.asprintf "%a" pp grp;
    [%expect {|
      let make ~format ?variables ?sendTelemetry ?things ?stuff () =
      {format; variables; sendTelemetry; things; stuff} |}]

end

module Stanza_getters_sig = struct

  let pp_field ~cyclic_field =
    Fmt.of_to_string (function Obj.{field_name; field_type; field_presence; field_container; _} as f ->
        let presence = match field_presence with | `Opt -> "option" | `Req -> "" in
        let container = match field_container with None -> "" | Some {container_type; _} -> container_type in
        match field_type with
        | `Builtin {builtin_type; _} ->
          Fmt.str "val %s : t -> %s %s %s" field_name builtin_type container presence
        | `User_defined {object_t; _} when cyclic_field = Some f ->
          Fmt.str "val %s : t -> %s %s %s" field_name object_t container presence
        | `User_defined {object_t; object_name; _} ->
          Fmt.str "val %s : t -> %s.%s %s %s" field_name object_name object_t container presence
      )

  let pp =
    Fmt.of_to_string (function Obj.{object_fields; _} as o ->
        let all_fields = Obj.ordered_fields object_fields in
        let cyclic_field =
          let xs = Obj.cyclic_fields o in
          List.nth_opt xs 0
        in
        let pp_fields = Fmt.list ~sep:(Fmt.any "\n\n") (pp_field ~cyclic_field) in
        Fmt.str "%a" pp_fields all_fields
      )

  let%expect_test "Check Stanza_getter_sig" =
    let grp = Obj.test_data () in
    print_endline @@ Format.asprintf "%a" pp grp;
    [%expect {|
      val format : t -> string list

      val variables : t -> Irmin.Contents.Json_value.t  option

      val sendTelemetry : t -> bool  option

      val things : t -> t tree option

      val stuff : t -> Stopped_event_enum.t  option |}]

end

module Stanza_getters_struct = struct

  let pp_field =
    Fmt.of_to_string (function Obj.{field_name; _} ->
        Fmt.str "let %s t = t.%s" field_name field_name
      )

  let pp =
    Fmt.of_to_string (function Obj.{object_fields; _} ->
        let all_fields = Obj.ordered_fields object_fields in
        let pp_fields = Fmt.list ~sep:(Fmt.any "\n\n") pp_field in
        Fmt.str "%a" pp_fields all_fields
      )

  let%expect_test "Check Stanza_getter_struct" =
    let grp = Obj.test_data () in
    print_endline @@ Format.asprintf "%a" pp grp;
    [%expect {|
      let format t = t.format

      let variables t = t.variables

      let sendTelemetry t = t.sendTelemetry

      let things t = t.things

      let stuff t = t.stuff |}]

end

module Stanza_enc_sig = struct

  let pp =
    Fmt.of_to_string (function Obj.{object_t=_; _} ->
        Fmt.str "val enc : t Data_encoding.t"
      )

  let%expect_test "Check Stanza_enc_sig" =
    let grp = Obj.test_data () in
    print_endline @@ Format.asprintf "%a" pp grp;
    [%expect {| val enc : t Data_encoding.t |}]
end

module Stanza_enc_struct = struct

  let mu_arg = "e"

  let pp_obj =
    let pp_field =
      let presence = function `Opt -> "opt" | `Req -> "req" in
      let container field_container = match field_container with None -> "" | Some Obj.{container_enc; _} -> container_enc in
      Fmt.of_to_string (function
          | Obj.{field_presence; field_container; field_dirty_name; field_type=`Builtin {builtin_enc; _}; _} ->
            Fmt.str "(%s \"%s\" (%s %s))" (presence field_presence) field_dirty_name (container field_container) builtin_enc

          | Obj.{field_presence; field_container; field_dirty_name; field_type=`User_defined {object_enc=`Raw enc; _}; _} ->
            Fmt.str "(%s \"%s\" (%s %s))" (presence field_presence) field_dirty_name (container field_container) enc

          | Obj.{field_presence; field_container; field_dirty_name; field_type=`User_defined {object_name; object_enc=`Qualified enc; _}; _} ->
            Fmt.str "(%s \"%s\" (%s %s.%s))" (presence field_presence) field_dirty_name (container field_container) object_name enc

          | Obj.{field_presence; field_container; field_dirty_name; field_type=`User_defined {object_enc=`Cyclic; _}; _} ->
            Fmt.str "(%s \"%s\" (%s %s))" (presence field_presence) field_dirty_name (container field_container) mu_arg

        )
    in
    Fmt.of_to_string (function Obj.{object_fields; _} ->
        let n = List.length object_fields in
        if n > 10 then
          raise @@ Invalid_argument (Fmt.str "pp_obj too many fields %d.  Please use pp_objn" n)
        else
          let pp_fields = Fmt.list ~sep:(Fmt.any "\n") pp_field in
          Fmt.str "(obj%d\n%a)" n pp_fields @@ Obj.ordered_fields object_fields
      )

  let pp_fields ~sep ~enclosing =
    Fmt.of_to_string (function Obj.{object_fields; _} ->
        let pp_list = Fmt.list ~sep:(Fmt.any sep) Fmt.string in
        let fs = List.map (function Obj.{field_name; _} -> field_name)
          @@ Obj.ordered_fields object_fields in
        Fmt.str "%s%a%s" (fst enclosing) pp_list fs (snd enclosing)
      )

  let pp =
    let pp_record = pp_fields ~sep:"; " ~enclosing:("{", "}") in
    let pp_tuple = pp_fields ~sep:", " ~enclosing:("(",")") in
    let pp_body =
      Fmt.of_to_string (fun st ->
          String.concat "\n" [
            "conv";
            Fmt.str "(fun %a -> \n %a)" pp_record st pp_tuple st;
            Fmt.str "(fun %a -> \n %a)" pp_tuple st pp_record st;
            Fmt.str "%a" pp_obj st;
          ]
        )
    in
    Fmt.of_to_string (
      function
      | st when Obj.has_cycle st ->
        String.concat "\n" [
          "let enc =";
          "let open Data_encoding in";
          Fmt.str "mu \"%s.%s\" (fun %s -> \n%a)"
            st.object_name st.object_t mu_arg pp_body st
        ]
      | st ->
        String.concat "\n" [
          "let enc =";
          "let open Data_encoding in";
          Fmt.str "%a" pp_body st
        ]
    )

  let%expect_test "Check Stanza_enc_struct" =
    let grp = Obj.test_data () in
    print_endline @@ Format.asprintf "%a" pp grp;
    [%expect {|
      let enc =
      let open Data_encoding in
      mu "Thing.t" (fun e ->
      conv
      (fun {format; variables; sendTelemetry; things; stuff} ->
       (format, variables, sendTelemetry, things, stuff))
      (fun (format, variables, sendTelemetry, things, stuff) ->
       {format; variables; sendTelemetry; things; stuff})
      (obj5
      (req "format_" (list string))
      (opt "_variables_" ( Data_encoding.json))
      (opt "sendTelemetry_" ( bool))
      (opt "things in a container" (tree_enc e))
      (opt "stuff" ( Stopped_event_enum.enc)))) |}]
end


module Printer_object = struct

  let pp_sig =
    Fmt.of_to_string (function o ->
        String.concat "\n\n" [
          Fmt.str "%a" Stanza_t_sig.pp o;
          Fmt.str "%a" Stanza_make_sig.pp o;
          Fmt.str "%a" Stanza_enc_sig.pp o;
          Fmt.str "%a" Stanza_getters_sig.pp o;
        ]
      )

  let pp_struct =
    Fmt.of_to_string (function o ->
        String.concat "\n\n" [
          Fmt.str "%a" Stanza_t_struct.pp o;
          Fmt.str "%a" Stanza_gen_struct.pp o;
          Fmt.str "%a" Stanza_make_struct.pp o;
          Fmt.str "%a" Stanza_enc_struct.pp o;
          Fmt.str "%a" Stanza_getters_struct.pp o;
        ]
      )

  let pp ~with_sig =
    Fmt.of_to_string (function o ->
        if with_sig then
          Fmt.str "module %s : sig\n%a\nend = struct\n%a\nend"
            o.Obj.object_name pp_sig o pp_struct o
        else
          Fmt.str "module %s = struct\n%a\nend"
            o.Obj.object_name pp_struct o
      )

  let%expect_test "Check Printer_object - no sig" =
    let grp = Obj.test_data () in
    print_endline @@ Format.asprintf "%a" (pp ~with_sig:false) grp;
    [%expect {|
      module Thing = struct
      type t = {
      format : (string list  [@gen Gen.gen_utf8_str_list]);
      variables : (Irmin.Contents.Json_value.t  option [@gen Gen.gen_json_opt]);
      sendTelemetry : (bool  option );
      things : (t tree option );
      stuff : (Stopped_event_enum.t  option )
      }
      [@@deriving irmin]

      let equal = Irmin.Type.(unstage (equal t))

      let merge = Irmin.Merge.idempotent t

      let gen = QCheck.Gen.(sized @@ fix (fun self n ->
      let basecase = oneofl [None; Some []] in
      let _gen_t =
      fun t ->
      let gg = tup5
      Gen.gen_utf8_str_list
      Gen.gen_json_opt
      (option bool)
      t
      (option Stopped_event_enum.gen) in
      map (fun ( format,variables,sendTelemetry,things,stuff ) -> { format;variables;sendTelemetry;things;stuff }) gg
      in match n with
      | 0 -> _gen_t basecase
      | n -> frequency
      [(1, _gen_t basecase);
      (1, let t = map (fun {things; _} -> things) @@ self (n / 2) in _gen_t t)]
      ))

      let arb = QCheck.make gen

      let make ~format ?variables ?sendTelemetry ?things ?stuff () =
      {format; variables; sendTelemetry; things; stuff}


      let enc =
      let open Data_encoding in
      mu "Thing.t" (fun e ->
      conv
      (fun {format; variables; sendTelemetry; things; stuff} ->
       (format, variables, sendTelemetry, things, stuff))
      (fun (format, variables, sendTelemetry, things, stuff) ->
       {format; variables; sendTelemetry; things; stuff})
      (obj5
      (req "format_" (list string))
      (opt "_variables_" ( Data_encoding.json))
      (opt "sendTelemetry_" ( bool))
      (opt "things in a container" (tree_enc e))
      (opt "stuff" ( Stopped_event_enum.enc))))

      let format t = t.format

      let variables t = t.variables

      let sendTelemetry t = t.sendTelemetry

      let things t = t.things

      let stuff t = t.stuff
      end |}]

  let%expect_test "Check Printer_object" =
    let grp = Obj.test_data () in
    print_endline @@ Format.asprintf "%a" (pp ~with_sig:true) grp;
    [%expect {|
      module Thing : sig
      type t [@@deriving irmin]

      val equal : t -> t -> bool

      val merge : t Irmin.Merge.t

      val gen : t QCheck.Gen.t

      val arb : t QCheck.arbitrary

      val make :
      format : string list ->
      ?variables : Irmin.Contents.Json_value.t  ->
      ?sendTelemetry : bool  ->
      ?things : t tree ->
      ?stuff : Stopped_event_enum.t  ->
      unit ->
      t

      val enc : t Data_encoding.t

      val format : t -> string list

      val variables : t -> Irmin.Contents.Json_value.t  option

      val sendTelemetry : t -> bool  option

      val things : t -> t tree option

      val stuff : t -> Stopped_event_enum.t  option
      end = struct
      type t = {
      format : (string list  [@gen Gen.gen_utf8_str_list]);
      variables : (Irmin.Contents.Json_value.t  option [@gen Gen.gen_json_opt]);
      sendTelemetry : (bool  option );
      things : (t tree option );
      stuff : (Stopped_event_enum.t  option )
      }
      [@@deriving irmin]

      let equal = Irmin.Type.(unstage (equal t))

      let merge = Irmin.Merge.idempotent t

      let gen = QCheck.Gen.(sized @@ fix (fun self n ->
      let basecase = oneofl [None; Some []] in
      let _gen_t =
      fun t ->
      let gg = tup5
      Gen.gen_utf8_str_list
      Gen.gen_json_opt
      (option bool)
      t
      (option Stopped_event_enum.gen) in
      map (fun ( format,variables,sendTelemetry,things,stuff ) -> { format;variables;sendTelemetry;things;stuff }) gg
      in match n with
      | 0 -> _gen_t basecase
      | n -> frequency
      [(1, _gen_t basecase);
      (1, let t = map (fun {things; _} -> things) @@ self (n / 2) in _gen_t t)]
      ))

      let arb = QCheck.make gen

      let make ~format ?variables ?sendTelemetry ?things ?stuff () =
      {format; variables; sendTelemetry; things; stuff}


      let enc =
      let open Data_encoding in
      mu "Thing.t" (fun e ->
      conv
      (fun {format; variables; sendTelemetry; things; stuff} ->
       (format, variables, sendTelemetry, things, stuff))
      (fun (format, variables, sendTelemetry, things, stuff) ->
       {format; variables; sendTelemetry; things; stuff})
      (obj5
      (req "format_" (list string))
      (opt "_variables_" ( Data_encoding.json))
      (opt "sendTelemetry_" ( bool))
      (opt "things in a container" (tree_enc e))
      (opt "stuff" ( Stopped_event_enum.enc))))

      let format t = t.format

      let variables t = t.variables

      let sendTelemetry t = t.sendTelemetry

      let things t = t.things

      let stuff t = t.stuff
      end |}]

end


module Printer_object_big = struct
  (* for when there are more than 10 fields
     Data_encoding only deals with encoders of max 10 fields,
     so need to work around that with inner modules *)

  (* NOTE the sig is the same as for small objects *)
  let pp_sig =
    Fmt.of_to_string (function o ->
        String.concat "\n\n" [
          Fmt.str "%a" Stanza_t_sig.pp o;
          Fmt.str "%a" Stanza_make_sig.pp o;
          Fmt.str "%a" Stanza_enc_sig.pp o;
          Fmt.str "%a" Stanza_getters_sig.pp o;
        ]
      )

  let grouping max_fields =
    function Obj.{object_name; object_fields; _} as o ->
      (* TODO deal with cyclic big objects *)
      assert (not @@ Obj.has_cycle o);

      let n = List.length object_fields in
      let ngroups = 1 + (n / max_fields) in
      let prs = List.init ngroups (fun i -> (i*max_fields, max_fields)) in
      let objs =
        prs
        |> List.map (fun (start_i, length) ->
            let n = min length @@ List.length object_fields - start_i in
            let object_fields = Array.(
                let arr = of_list object_fields in
                sub arr start_i n |> to_list
              )
            in
            let object_name = Fmt.str "%s_%d" object_name start_i in
            {o with object_name; object_fields}
          )
      in
      objs

  (* need to group into bracketed pairs *)
  let rec aux_brkts ~sep ~pp = function
    | x :: [y] -> Fmt.str "(%a%s %a)" pp x sep pp y
    | ln :: rest ->
      let lns = aux_brkts ~sep ~pp rest in
      Fmt.str "(%a%s %s)" pp ln sep lns
    | [] -> ""

  let rec aux ~pp ~fname =
    let sep = "@@" in
    function
    | x :: [y] -> Fmt.str "%s %a %a" fname pp x pp y
    | ln :: rest ->
      let lns = aux ~pp ~fname rest in
      Fmt.str "%s %a \n%s %s" fname pp ln sep lns
    | [] -> ""

  let pp_inner_structs ~max_fields fmt o =
    let objs = grouping max_fields o in
    Fmt.list ~sep:(Fmt.any "\n\n") (Printer_object.pp ~with_sig:false) fmt objs

  let pp_ts ~max_fields =
    let pp_t =
      let pp =
        Fmt.of_to_string (function Obj.{object_name; object_t; _} ->
            Fmt.str "%s.%s" object_name object_t
          )
      in
      Fmt.of_to_string (aux_brkts ~sep:" *" ~pp)
    in
    let pp_arg ~tok =
      let pp =
        Fmt.of_to_string (function Obj.{object_name; _} ->
            Fmt.str "%s_%s" tok object_name
          )
      in
      Fmt.of_to_string (aux_brkts ~sep:"," ~pp)
    in

    let pp_equal ~tok1 ~tok2 =
      let pp ~tok =
        Fmt.of_to_string (function Obj.{object_name; _} ->
            Fmt.str "%s_%s" tok object_name
          )
      in
      Fmt.of_to_string (function Obj.{object_name; _} as o->
          Fmt.str "%s.equal %a %a" object_name (pp ~tok:tok1) o (pp ~tok:tok2) o
        )
    in

    let pp_merge =
      let pp =
        Fmt.of_to_string (function Obj.{object_name; _} ->
            Fmt.str "%s.merge" object_name
          )
      in
      Fmt.of_to_string (aux ~pp ~fname:"pair") in

    Fmt.of_to_string (fun o ->
        let objs = grouping max_fields o in
        let ss = [
          Fmt.str "type t = %a\n[%s irmin, qcheck]" pp_t objs Consts.deriving_str;
          Fmt.str "let equal\n%a\n%a = \n%a"
            (pp_arg ~tok:"s") objs
            (pp_arg ~tok:"t") objs
            (Fmt.list ~sep:(Fmt.any "\n && ") (pp_equal ~tok1:"s" ~tok2:"t")) objs;
          Fmt.str "let merge = Irmin.Merge.(\n%a\n)" pp_merge objs;
        ] in
        String.concat "\n\n" ss
      )

  let pp_encs ~max_fields =
    let pp_encs =
      let pp =
        Fmt.of_to_string (function Obj.{object_name; _} ->
            Fmt.str "%s.enc" object_name
          )
      in
      Fmt.of_to_string (aux ~pp ~fname:"merge_objs")
    in

    Fmt.of_to_string (fun o ->
        let objs = grouping max_fields o in
        let ss = [
          Fmt.str "let enc = ";
          Fmt.str "let open Data_encoding in ";
          Fmt.str "%a" pp_encs objs;
        ] in
        String.concat "\n" ss
      )

  let pp_makes ~max_fields =
    let pp_args =
      Fmt.list ~sep:(Fmt.any " ") Stanza_make_struct.pp_field_upper
    in

    let pp_make =
      Fmt.of_to_string (fun o ->
          let all_fields = Obj.ordered_fields o.Obj.object_fields in
          Fmt.str "let make %a () = " pp_args all_fields
        )
    in

    let pp_t =
      Fmt.of_to_string (
        function Obj.{object_name; _} ->
          Fmt.str "t_%s" object_name
      )
    in

    let pp_ts =
      Fmt.of_to_string (aux_brkts ~sep:"," ~pp:pp_t)
    in

    let pp_inners =
      let pp_inner =
        Fmt.of_to_string (function Obj.{object_name; object_fields; _} as o ->
            let all_fields = Obj.ordered_fields object_fields in
            Fmt.str "let %a =\n%s.make \n%a ()\n in" pp_t o object_name pp_args all_fields
          )
      in
      Fmt.list ~sep:(Fmt.any "\n\n") pp_inner
    in

    Fmt.of_to_string (fun o ->
        let objs = grouping max_fields o in
        Fmt.str "%a\n%a\n\n%a"
          pp_make o
          pp_inners objs
          pp_ts objs
      )

  let pp_getters ~max_fields = Fmt.of_to_string (function o ->
      let objs = grouping max_fields o in

      let pp_t = Fmt.of_to_string (fun Obj.{object_name; _} ->
          Fmt.str "_t_%s" object_name
        )
      in
      let pp_ts = Fmt.of_to_string (aux_brkts ~sep:"," ~pp:pp_t) in

      let pp_field ~obj=
        Fmt.of_to_string (function Obj.{field_name; _} ->
            Fmt.str "let %s %a =\n  %s.%s %a"
              field_name
              pp_ts objs
              obj.Obj.object_name
              field_name
              pp_t obj
          )
      in

      let pp_fields =
        Fmt.of_to_string (function Obj.{object_fields; _} as o ->
            Fmt.str "%a" (Fmt.list ~sep:(Fmt.any "\n\n") @@ pp_field ~obj:o) object_fields
          )
      in

      Fmt.str "%a" (Fmt.list ~sep:(Fmt.any "\n\n") pp_fields) objs
    )

  let pp_struct ~max_fields =
    Fmt.of_to_string (fun o ->
        Fmt.str "%a\n%a\n\n%a\n\n%a\n\n%a"
          (pp_inner_structs ~max_fields) o
          (pp_ts ~max_fields) o
          (pp_encs ~max_fields) o
          (pp_makes ~max_fields) o
          (pp_getters ~max_fields) o
      )

  let pp_module ~max_fields =
    Fmt.of_to_string (function o ->
        Fmt.str
          "module %s : sig\n%a\nend = struct\n%a\nend"
          o.Obj.object_name
          pp_sig o
          (pp_struct ~max_fields) o
      )

  let pp = pp_module ~max_fields:10

  let%expect_test "Check Printer_object_big" =
    let test_data () = Obj.(
        let object_name = "Big_thing" in
        let object_t = "t" in
        let object_enc = `Qualified "enc" in
        let object_fields =
          List.init 8 (fun i ->
              { field_name=Fmt.str "format%d" i;
                field_dirty_name=Fmt.str "format_%d" i;
                field_type=`Builtin {builtin_type="string";
                                     builtin_enc="string"};
                field_presence=`Req;
                field_container=Some {container_type="list";
                                      container_enc="list"};
                field_gen_container=None;
                field_index=i };
            )
        in
        {object_name; object_t; object_enc; object_fields}
      )
    in

    let grp = test_data () in
    print_endline @@ Format.asprintf "%a" (pp_module ~max_fields:3) grp;
    [%expect {|
      module Big_thing : sig
      type t [@@deriving irmin]

      val equal : t -> t -> bool

      val merge : t Irmin.Merge.t

      val gen : t QCheck.Gen.t

      val arb : t QCheck.arbitrary

      val make :
      format0 : string list ->
      format1 : string list ->
      format2 : string list ->
      format3 : string list ->
      format4 : string list ->
      format5 : string list ->
      format6 : string list ->
      format7 : string list ->
      unit ->
      t

      val enc : t Data_encoding.t

      val format0 : t -> string list

      val format1 : t -> string list

      val format2 : t -> string list

      val format3 : t -> string list

      val format4 : t -> string list

      val format5 : t -> string list

      val format6 : t -> string list

      val format7 : t -> string list
      end = struct
      module Big_thing_0 = struct
      type t = {
      format0 : (string list  );
      format1 : (string list  );
      format2 : (string list  )
      }
      [@@deriving irmin, qcheck]

      let equal = Irmin.Type.(unstage (equal t))

      let merge = Irmin.Merge.idempotent t



      let make ~format0 ~format1 ~format2 () =
      {format0; format1; format2}


      let enc =
      let open Data_encoding in
      conv
      (fun {format0; format1; format2} ->
       (format0, format1, format2))
      (fun (format0, format1, format2) ->
       {format0; format1; format2})
      (obj3
      (req "format_0" (list string))
      (req "format_1" (list string))
      (req "format_2" (list string)))

      let format0 t = t.format0

      let format1 t = t.format1

      let format2 t = t.format2
      end

      module Big_thing_3 = struct
      type t = {
      format3 : (string list  );
      format4 : (string list  );
      format5 : (string list  )
      }
      [@@deriving irmin, qcheck]

      let equal = Irmin.Type.(unstage (equal t))

      let merge = Irmin.Merge.idempotent t



      let make ~format3 ~format4 ~format5 () =
      {format3; format4; format5}


      let enc =
      let open Data_encoding in
      conv
      (fun {format3; format4; format5} ->
       (format3, format4, format5))
      (fun (format3, format4, format5) ->
       {format3; format4; format5})
      (obj3
      (req "format_3" (list string))
      (req "format_4" (list string))
      (req "format_5" (list string)))

      let format3 t = t.format3

      let format4 t = t.format4

      let format5 t = t.format5
      end

      module Big_thing_6 = struct
      type t = {
      format6 : (string list  );
      format7 : (string list  )
      }
      [@@deriving irmin, qcheck]

      let equal = Irmin.Type.(unstage (equal t))

      let merge = Irmin.Merge.idempotent t



      let make ~format6 ~format7 () =
      {format6; format7}


      let enc =
      let open Data_encoding in
      conv
      (fun {format6; format7} ->
       (format6, format7))
      (fun (format6, format7) ->
       {format6; format7})
      (obj2
      (req "format_6" (list string))
      (req "format_7" (list string)))

      let format6 t = t.format6

      let format7 t = t.format7
      end
      type t = (Big_thing_0.t * (Big_thing_3.t * Big_thing_6.t))
      [@@deriving irmin, qcheck]

      let equal
      (s_Big_thing_0, (s_Big_thing_3, s_Big_thing_6))
      (t_Big_thing_0, (t_Big_thing_3, t_Big_thing_6)) =
      Big_thing_0.equal s_Big_thing_0 t_Big_thing_0
       && Big_thing_3.equal s_Big_thing_3 t_Big_thing_3
       && Big_thing_6.equal s_Big_thing_6 t_Big_thing_6

      let merge = Irmin.Merge.(
      pair Big_thing_0.merge
      @@ pair Big_thing_3.merge Big_thing_6.merge
      )

      let enc =
      let open Data_encoding in
      merge_objs Big_thing_0.enc
      @@ merge_objs Big_thing_3.enc Big_thing_6.enc

      let make ~format0 ~format1 ~format2 ~format3 ~format4 ~format5 ~format6 ~format7 () =
      let t_Big_thing_0 =
      Big_thing_0.make
      ~format0 ~format1 ~format2 ()
       in

      let t_Big_thing_3 =
      Big_thing_3.make
      ~format3 ~format4 ~format5 ()
       in

      let t_Big_thing_6 =
      Big_thing_6.make
      ~format6 ~format7 ()
       in

      (t_Big_thing_0, (t_Big_thing_3, t_Big_thing_6))

      let format0 (_t_Big_thing_0, (_t_Big_thing_3, _t_Big_thing_6)) =
        Big_thing_0.format0 _t_Big_thing_0

      let format1 (_t_Big_thing_0, (_t_Big_thing_3, _t_Big_thing_6)) =
        Big_thing_0.format1 _t_Big_thing_0

      let format2 (_t_Big_thing_0, (_t_Big_thing_3, _t_Big_thing_6)) =
        Big_thing_0.format2 _t_Big_thing_0

      let format3 (_t_Big_thing_0, (_t_Big_thing_3, _t_Big_thing_6)) =
        Big_thing_3.format3 _t_Big_thing_3

      let format4 (_t_Big_thing_0, (_t_Big_thing_3, _t_Big_thing_6)) =
        Big_thing_3.format4 _t_Big_thing_3

      let format5 (_t_Big_thing_0, (_t_Big_thing_3, _t_Big_thing_6)) =
        Big_thing_3.format5 _t_Big_thing_3

      let format6 (_t_Big_thing_0, (_t_Big_thing_3, _t_Big_thing_6)) =
        Big_thing_6.format6 _t_Big_thing_6

      let format7 (_t_Big_thing_0, (_t_Big_thing_3, _t_Big_thing_6)) =
        Big_thing_6.format7 _t_Big_thing_6
      end |}]

end


