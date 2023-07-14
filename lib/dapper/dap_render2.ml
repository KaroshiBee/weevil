
module Sp = Dap_specs
module CommandHelper = Dap_dfs.CommandHelper
module EventHelper = Dap_dfs.EventHelper
module Dfs = Dap_dfs.Dfs

(* TODO want to be able to easily add new derivings *)

module Enum = struct

  type t = {
    element_name : string
  ; element_dirty_name : string
  ; element_index : int
  } [@@deriving show, eq]

  and enum = {
    enum_name : string
  ; enum_t : string
  ; enum_enc : string
  ; enum_type : [ `Open | `Closed ] (* whether it allows for `| Other of string` *)
  ; enum_elements : t list
  }

  let ordered_elements els =
    els
    |> List.sort (fun x y -> compare x.element_index y.element_index)

  let test_data ~enum_type () =
    let enum_name = "Stopped_event_enum" in
    let enum_t = "t" in
    let enum_enc = "enc" in
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
    {enum_name; enum_t; enum_enc; enum_type; enum_elements}

end

module Field = struct

  type t = {
    field_name : string
  ; field_dirty_name : string
  ; field_type : [ `Builtin of builtin | `Struct of object_ | `Enum of Enum.enum ]
  ; field_presence : [`Opt | `Req ]
  ; field_container : string option (* we only deal with one container: t list  *)
  ; field_index : int
  } [@@deriving show, eq]

  and object_ = {
    object_name : string (* what the struct would be called ie Thing *)
  ; object_t : string (* what 't' would be called i.e. for Thing.t *)
  ; object_enc : [ `Cyclic of string | `Raw of string | `Qualified of string ] (* what 'enc' would be called *)
  ; object_fields : t list
  } [@@deriving show, eq ]

  and builtin = {
    builtin_name : string
  ; builtin_enc : string
  } [@@deriving show, eq ]

  let ordered_fields fields =
    fields
    |> List.sort (fun x y -> compare x.field_index y.field_index)

  let test_data () =
    let object_name = "Thing" in
    let object_t = "t" in
    let object_enc = `Qualified "enc" in
    let object_fields = [
      { field_name="variables";
        field_dirty_name="_variables_";
        field_type=`Struct {object_name="Irmin.Contents.Json_value";
                            object_t="t";
                            object_enc = `Qualified "json";
                            object_fields=[]};
        field_presence=`Opt;
        field_container=None;
        field_index=2 };
      { field_name="format";
        field_dirty_name="format_";
        field_type=`Builtin {builtin_name="string";
                             builtin_enc="string"};
        field_presence=`Req;
        field_container=Some "list";
        field_index=1 };
      { field_name="sendTelemetry";
        field_dirty_name="sendTelemetry_";
        field_type=`Builtin {builtin_name="bool";
                             builtin_enc="bool"};
        field_presence=`Opt;
        field_container=None;
        field_index=3 };
      { field_name="things";
        field_dirty_name="things in a list";
        field_type=`Struct {object_name="Thing";
                            object_t="t";
                            object_enc = `Cyclic "list";
                            object_fields=[]};
        field_presence=`Opt;
        field_container=Some "list";
        field_index=4 };
      { field_name="stuff";
        field_dirty_name="stuff";
        field_type=`Enum (Enum.test_data ~enum_type:`Closed ());
        field_presence=`Opt;
        field_container=None;
        field_index=5 };
    ]
    in
    {object_name; object_t; object_enc; object_fields}
end

module type PP_struct = sig
  val pp : Format.formatter -> 'a -> unit
end

(* NOTE these @@ are awkard when used directly in format string *)
let deriving_str = "@@deriving"

module Stanza_t_sig = struct

  (* TODO compose in the deriving irmin *)
  let pp =
    Fmt.of_to_string (function Field.{object_t; _} ->
        let ss = [
          Fmt.str "type %s [%s irmin]" object_t deriving_str;
          Fmt.str "val equal : %s -> %s -> bool" object_t object_t;
          Fmt.str "val merge : %s option Irmin.Merge.t" object_t;
        ] in
        String.concat "\n\n" ss
      )

  let%expect_test "Check Stanza_t_sig" =
    let grp = Field.test_data () in
    print_endline @@ Format.asprintf "%a" pp grp;
    [%expect {|
      type t [@@deriving irmin]

      val equal : t -> t -> bool

      val merge : t option Irmin.Merge.t |}]

end

module Stanza_t_struct = struct

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
  (* let equal = Irmin.Type.(unstage (equal t)) *)
  (* let merge = Irmin.Merge.(option (idempotent t)) *)

  let pp_field =
    Fmt.of_to_string (function Field.{field_name; field_type; field_presence; field_container; _} ->
        let presence = match field_presence with | `Opt -> "option" | `Req -> "" in
        let container = match field_container with None -> "" | Some l -> l in
        match field_type with
        | `Builtin {builtin_name; _} ->
          Fmt.str "%s : %s %s %s" field_name builtin_name container presence
        | `Struct {object_t; _} ->
          Fmt.str "%s : %s %s %s" field_name object_t container presence
        | `Enum Enum.{enum_name; enum_t; _} ->
          Fmt.str "%s : %s.%s %s %s" field_name enum_name enum_t container presence
      )

  let pp =
    Fmt.of_to_string (function Field.{object_fields; object_t; _} ->
        let fields = Field.ordered_fields object_fields in
        let pp_fields = Fmt.list ~sep:(Fmt.any ";\n") pp_field in
        let ss = [
          Fmt.str "type %s = {\n%a\n}\n[%s irmin]" object_t pp_fields fields deriving_str;
          Fmt.str "let equal = Irmin.Type.(unstage (equal %s))" object_t;
          Fmt.str "let merge = Irmin.Merge.(option (idempotent %s))" object_t;
        ] in
        String.concat "\n\n" ss
      )

  let%expect_test "Check Stanza_struct" =
    let grp = Field.test_data () in
    print_endline @@ Format.asprintf "%a" pp grp;
    [%expect {|
      type t = {
      format : string list ;
      variables : t  option;
      sendTelemetry : bool  option;
      things : t list option;
      stuff : Stopped_event_enum.t  option
      }
      [@@deriving irmin]

      let equal = Irmin.Type.(unstage (equal t))

      let merge = Irmin.Merge.(option (idempotent t)) |}]

end

module Stanza_make_sig = struct
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
    Fmt.of_to_string (function Field.{field_name; field_type; field_presence; field_container; _} ->
        let presence = match field_presence with | `Opt -> "?" | `Req -> "" in
        let container = match field_container with None -> "" | Some l -> l in
        match field_type with
        | `Builtin {builtin_name; _} ->
          Fmt.str "%s%s : %s %s" presence field_name builtin_name container
        | `Struct {object_t; _} ->
          Fmt.str "%s%s : %s %s" presence field_name object_t container
        | `Enum Enum.{enum_name; enum_t; _} ->
          Fmt.str "%s%s : %s.%s %s" presence field_name enum_name enum_t container
      )

  let pp =
    Fmt.of_to_string (function Field.{object_fields; object_t; _} ->
        let fields = Field.ordered_fields object_fields in
        let pp_fields = Fmt.list ~sep:(Fmt.any " -> \n") pp_field in
        Fmt.str "val make : \n%a -> \nunit -> \n%s" pp_fields fields object_t
      )

  let%expect_test "Check Stanza_make_sig" =
    let grp = Field.test_data () in
    print_endline @@ Format.asprintf "%a" pp grp;
    [%expect {|
      val make :
      format : string list ->
      ?variables : t  ->
      ?sendTelemetry : bool  ->
      ?things : t list ->
      ?stuff : Stopped_event_enum.t  ->
      unit ->
      t |}]

end

module Stanza_make_struct = struct
(*
   e.g.
    let make ~id ~format ?variables ?sendTelemetry ?showUser ?url ?urlLabel () =
      {id; format; variables; sendTelemetry; showUser; url; urlLabel}
*)

  let pp_field_upper =
    Fmt.of_to_string (function Field.{field_name; field_presence; _} ->
        let presence = match field_presence with | `Opt -> "?" | `Req -> "~" in
        Fmt.str "%s%s" presence field_name
      )

  let pp_field_lower =
    Fmt.of_to_string (function Field.{field_name; _} ->
        Fmt.str "%s" field_name
      )

  let pp =
    Fmt.of_to_string (function Field.{object_fields; _} ->
        let all_fields = Field.ordered_fields object_fields in
        let pp_upper = Fmt.list ~sep:(Fmt.any " ") pp_field_upper in
        let pp_lower = Fmt.list ~sep:(Fmt.any "; ") pp_field_lower in
        Fmt.str "let make %a () =\n{%a}\n" pp_upper all_fields pp_lower all_fields
      )

  let%expect_test "Check Stanza_make_struct" =
    let grp = Field.test_data () in
    print_endline @@ Format.asprintf "%a" pp grp;
    [%expect {|
      let make ~format ?variables ?sendTelemetry ?things ?stuff () =
      {format; variables; sendTelemetry; things; stuff} |}]

end

module Stanza_getters_sig = struct
(*
   e.g.
    val id : t -> int

    val format : t -> string

    val variables : t -> Irmin.Contents.Json_value.t option
*)

  let pp_field ~object_t =
    Fmt.of_to_string (function Field.{field_name; field_type; field_presence; field_container; _} ->
        let presence = match field_presence with | `Opt -> "option" | `Req -> "" in
        let container = match field_container with None -> "" | Some l -> l in
        match field_type with
        | `Builtin {builtin_name; _} ->
          Fmt.str "val %s : %s -> %s %s %s" field_name object_t builtin_name container presence
        | `Struct {object_t; _} ->
          Fmt.str "val %s : %s -> %s %s %s" field_name object_t object_t container presence
        | `Enum Enum.{enum_name; enum_t; _} ->
          Fmt.str "val %s : %s -> %s.%s %s %s" field_name object_t enum_name enum_t container presence
      )

  let pp =
    Fmt.of_to_string (function Field.{object_fields; object_t; _} ->
        let all_fields = Field.ordered_fields object_fields in
        let pp_fields = Fmt.list ~sep:(Fmt.any "\n\n") (pp_field ~object_t) in
        Fmt.str "%a" pp_fields all_fields
      )

  let%expect_test "Check Stanza_getter_sig" =
    let grp = Field.test_data () in
    print_endline @@ Format.asprintf "%a" pp grp;
    [%expect {|
      val format : t -> string list

      val variables : t -> t  option

      val sendTelemetry : t -> bool  option

      val things : t -> t list option

      val stuff : t -> Stopped_event_enum.t  option |}]

end

module Stanza_getters_struct = struct
(*
   e.g.
    let id t = t.id

    let format t = t.format

    let variables t = t.variables
*)

  let pp_field ~object_t =
    Fmt.of_to_string (function Field.{field_name; _} ->
        Fmt.str "let %s %s = %s.%s" field_name object_t object_t field_name
      )

  let pp =
    Fmt.of_to_string (function Field.{object_fields; object_t; _} ->
        let all_fields = Field.ordered_fields object_fields in
        let pp_fields = Fmt.list ~sep:(Fmt.any "\n\n") (pp_field ~object_t) in
        Fmt.str "%a" pp_fields all_fields
      )

  let%expect_test "Check Stanza_getter_struct" =
    let grp = Field.test_data () in
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
    Fmt.of_to_string (function Field.{object_t; _} ->
        Fmt.str "val enc : %s Data_encoding.t" object_t
      )

  let%expect_test "Check Stanza_enc_sig" =
    let grp = Field.test_data () in
    print_endline @@ Format.asprintf "%a" pp grp;
    [%expect {| val enc : t Data_encoding.t |}]
end

module Stanza_enc_struct = struct

  (* e.g. *)
  (* let enc = *)
  (*   let open Data_encoding in *)
  (*   (\* Message.t *\) *)
  (*   conv *)
  (*     (fun {id; format; variables; sendTelemetry; showUser; url; urlLabel} -> *)
  (*       (id, format, variables, sendTelemetry, showUser, url, urlLabel)) *)
  (*     (fun (id, format, variables, sendTelemetry, showUser, url, urlLabel) -> *)
  (*       {id; format; variables; sendTelemetry; showUser; url; urlLabel}) *)
  (*     (obj7 *)
  (*        (req "id" int31) *)
  (*        (req "format" string) *)
  (*        (opt "variables" Irmin.Contents.Json_value.json) *)
  (*        (opt "sendTelemetry" bool) *)
  (*        (opt "showUser" bool) *)
  (*        (opt "url" string) *)
  (*        (opt "urlLabel" string)) *)

  let mu_arg = "e"

  let pp_obj =
    let pp_field =
      let presence = function `Opt -> "opt" | `Req -> "req" in
      Fmt.of_to_string (function
          | Field.{field_presence; field_dirty_name; field_type=`Builtin {builtin_enc; _}; _} ->
            Fmt.str "(%s \"%s\" %s)" (presence field_presence) field_dirty_name builtin_enc
          | Field.{field_presence; field_dirty_name; field_type=`Struct {object_enc=`Raw enc; _}; _} ->
            Fmt.str "(%s \"%s\" %s)" (presence field_presence) field_dirty_name enc
          | Field.{field_presence; field_dirty_name; field_type=`Struct {object_name; object_enc=`Qualified enc; _}; _} ->
            Fmt.str "(%s \"%s\" %s.%s)" (presence field_presence) field_dirty_name object_name enc
          | Field.{field_presence; field_dirty_name; field_type=`Struct {object_enc=`Cyclic enc; _}; _} ->
            Fmt.str "(%s \"%s\" (%s %s))" (presence field_presence) field_dirty_name enc mu_arg
          | Field.{field_presence; field_dirty_name; field_type=`Enum {enum_name; enum_enc; _}; _} ->
            Fmt.str "(%s \"%s\" %s.%s)" (presence field_presence) field_dirty_name enum_name enum_enc
        )
    in
    Fmt.of_to_string (function Field.{object_fields; _} ->
        let n = List.length object_fields in
        if n > 10 then
          raise @@ Invalid_argument (Fmt.str "pp_obj too many fields %d.  Please use pp_objn" n)
        else
          let pp_fields = Fmt.list ~sep:(Fmt.any "\n") pp_field in
          Fmt.str "(obj%d\n%a)" n pp_fields @@ Field.ordered_fields object_fields
      )

  let pp_fields ~sep ~enclosing =
    Fmt.of_to_string (function Field.{object_fields; _} ->
        let pp_list = Fmt.list ~sep:(Fmt.any sep) Fmt.string in
        let fs = List.map (function Field.{field_name; _} -> field_name)
            @@ Field.ordered_fields object_fields in
        Fmt.str "%s%a%s" (fst enclosing) pp_list fs (snd enclosing)
      )

  (* NOTE only go one level deep *)
  let has_cycle =
    function Field.{object_fields; _} ->
      List.exists
        (function
          | Field.{field_type=`Struct {object_enc=`Cyclic _; _}; _} -> true
          | _ -> false
        ) object_fields

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
        | st when has_cycle st ->
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
    let grp = Field.test_data () in
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
      (req "format_" string)
      (opt "_variables_" Irmin.Contents.Json_value.json)
      (opt "sendTelemetry_" bool)
      (opt "things in a list" (list e))
      (opt "stuff" Stopped_event_enum.enc))) |}]
end


(* NOTE dont need a sig for the enum types, inferred one is fine *)
module Stanza_enum_t_struct = struct

  let pp =
    Fmt.of_to_string (function Enum.{enum_t; enum_elements; enum_type; _} ->
        let pp_element =
          Fmt.of_to_string (function Enum.{element_name; _} ->
              Fmt.str "| %s" element_name
            )
        in
        let pp_elements =
          Fmt.list ~sep:(Fmt.any "\n") pp_element
        in
        let elements = Enum.ordered_elements enum_elements in
        let deriving = Fmt.str "[%s irmin]" deriving_str in
        let last = function
          | `Open -> ["| Other of string"; deriving]
          | `Closed -> [deriving]
        in
        String.concat "\n" @@ List.concat [
          [Fmt.str "type %s =\n%a" enum_t pp_elements elements];
          last enum_type;
          [Fmt.str "let equal = Irmin.Type.(unstage (equal %s))" enum_t;
           Fmt.str "let merge = Irmin.Merge.(option (idempotent %s))" enum_t];
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
      [@@deriving irmin]
      let equal = Irmin.Type.(unstage (equal t))
      let merge = Irmin.Merge.(option (idempotent t)) |}]

  let%expect_test "Check Stanza_enum_t_struct Closed" =
    let grp = Enum.test_data ~enum_type:`Closed () in
    print_endline @@ Format.asprintf "%a" pp grp;
    [%expect {|
      type t =
      | Step
      | Breakpoint
      | Exception
      [@@deriving irmin]
      let equal = Irmin.Type.(unstage (equal t))
      let merge = Irmin.Merge.(option (idempotent t)) |}]

end

module Stanza_enum_enc_struct = struct

  let pp =
    Fmt.of_to_string (function Enum.{enum_name; enum_enc; enum_elements; enum_type; _} ->
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
          Fmt.str "let %s =" enum_enc;
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
      [@@deriving irmin]
      let equal = Irmin.Type.(unstage (equal t))
      let merge = Irmin.Merge.(option (idempotent t))

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
      [@@deriving irmin]
      let equal = Irmin.Type.(unstage (equal t))
      let merge = Irmin.Merge.(option (idempotent t))

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
          Fmt.str "%a" Stanza_make_struct.pp o;
          Fmt.str "%a" Stanza_enc_struct.pp o;
          Fmt.str "%a" Stanza_getters_struct.pp o;
        ]
      )

  let pp =
    Fmt.of_to_string (function o ->
        Fmt.str "module %s : sig\n%a\nend = struct\n%a\nend"
          o.Field.object_name pp_sig o pp_struct o
      )

  let%expect_test "Check Printer_object" =
    let grp = Field.test_data () in
    print_endline @@ Format.asprintf "%a" pp grp;
    [%expect {|
      module Thing : sig
      type t [@@deriving irmin]

      val equal : t -> t -> bool

      val merge : t option Irmin.Merge.t

      val make :
      format : string list ->
      ?variables : t  ->
      ?sendTelemetry : bool  ->
      ?things : t list ->
      ?stuff : Stopped_event_enum.t  ->
      unit ->
      t

      val enc : t Data_encoding.t

      val format : t -> string list

      val variables : t -> t  option

      val sendTelemetry : t -> bool  option

      val things : t -> t list option

      val stuff : t -> Stopped_event_enum.t  option
      end = struct
      type t = {
      format : string list ;
      variables : t  option;
      sendTelemetry : bool  option;
      things : t list option;
      stuff : Stopped_event_enum.t  option
      }
      [@@deriving irmin]

      let equal = Irmin.Type.(unstage (equal t))

      let merge = Irmin.Merge.(option (idempotent t))

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
      (req "format_" string)
      (opt "_variables_" Irmin.Contents.Json_value.json)
      (opt "sendTelemetry_" bool)
      (opt "things in a list" (list e))
      (opt "stuff" Stopped_event_enum.enc)))

      let format t = t.format

      let variables t = t.variables

      let sendTelemetry t = t.sendTelemetry

      let things t = t.things

      let stuff t = t.stuff
      end |}]

end
