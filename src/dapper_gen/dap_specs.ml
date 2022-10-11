module Q = Json_query

let unweird_name ?(capitalize=false) name =
  let n = match (String.lowercase_ascii name) with
  | "type" | "module" | "lazy" | "variable" -> Printf.sprintf "%s_" name
  | "null" -> "empty_"
  | _ -> name
  in
  n
  |> Stringext.replace_all ~pattern:" " ~with_:"_"
  |> (fun s -> if capitalize then String.capitalize_ascii s else s)

  let rec make_module_name = function
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
      -> make_module_name rest
    | `Field f :: rest -> (
      match make_module_name rest with
      | "" -> unweird_name f
      | s -> Printf.sprintf "%s_%s" f s
    )
    | [] -> ""


module Field_spec = struct
  type t = {
    safe_name:string; (* for being a record name in ocaml *)
    dirty_name: string; (* what it gets written back to json field as *)
    path: Q.path; [@printer Q.print_path_as_json_pointer]
    module_name: string;
    type_:string; (* the ocaml type *)
    enc_: string; (* the encoding function name *)
    required:bool;
    cyclic: bool;
    seq: bool;
  } [@@deriving show]

  let make ~path ~dirty_name ~required ?(module_name="") ?(type_="") ?(enc_="") ?(cyclic=false) ?(seq=false) () =
    let safe_name = unweird_name dirty_name in
    let module_name = unweird_name module_name in
    {
      safe_name;
      dirty_name;
      path;
      module_name;
      type_;
      enc_;
      required;
      cyclic;
      seq;
    }
end

module Obj_spec = struct

  (*
     module Obj_name = struct

     type t = {
       field[0].safe_name: field[0].type_;
       field[1].safe_name: field[1].type_ option (if not field[1].required) ;
       etc...
     }

     let make ~field[0].safe_name ?field[1].safe_name etc... () =
     { field[0].safe_name; field[1].safe_name; etc...}

     let enc =
     conv
     (fun {...} -> (...))
     (fun (...) -> {...})
     (objN
       (req field[0].dirty_name field[0].enc)
       (opt field[1].dirty_name field[1].enc)
       etc...
     )
     end

     NOTES
     if its a big object then need to plit into a tuple of obj10's
     if it is cyclic then need to use the fixpoint encoder
 *)

  type t = {
    safe_name:string; (* for being a record name in ocaml *)
    dirty_name: string; (* what it gets written back to json field as *)
    path: Q.path; [@printer Q.print_path_as_json_pointer]
    fields: Field_spec.t list;
    is_cyclic: bool;
  } [@@deriving show]

  let of_path ~dirty_name ~path ?(fields=[]) ?(is_cyclic=false) () =
    let safe_name = unweird_name dirty_name in
    {safe_name; dirty_name; path; fields; is_cyclic}

  let is_big t =
    List.length t.fields > 10

  let append_fields_front t other =
    {t with fields = other.fields @ t.fields}

end


module Enum_spec = struct

  type enum_val = {
    safe_name: string;
    dirty_name: string;
  } [@@deriving show]

  type t = {
    safe_name:string; (* for being a record name in ocaml *)
    dirty_name: string; (* what it gets written back to json field as *)
    path: Q.path; [@printer Q.print_path_as_json_pointer]
    enums: enum_val list;
    suggested: bool;
  } [@@deriving show]

  let of_path ~dirty_name ~path ?(dirty_names=[]) ?(suggested=false) () =
    let safe_name = unweird_name dirty_name in
    let enums =
      dirty_names
      |> List.map (fun dirty_name ->
          let safe_name = unweird_name ~capitalize:true dirty_name in
          {safe_name; dirty_name}
        )
    in
    {safe_name; dirty_name; path; enums; suggested}

  let set_enums t ~dirty_names =
    let enums =
      dirty_names
      |> List.map (fun dirty_name ->
          let safe_name = unweird_name ~capitalize:true dirty_name in
          {safe_name; dirty_name}
        )
    in
    {t with enums}

  let append_enum t ~dirty_name =
    let safe_name = unweird_name ~capitalize:true dirty_name in
    {t with enums={safe_name; dirty_name}::t.enums}

  let append_enums t ~enums =
    let dirty_names = List.concat [
          List.map (fun (nm:enum_val) -> nm.dirty_name) enums;
          List.map (fun (nm:enum_val) -> nm.dirty_name) t.enums
        ] in
    set_enums t ~dirty_names

  let is_command t =
    1 = List.length t.enums &&
    match (List.hd @@ List.rev @@ t.path) with
    | `Field "command" -> true
    | _ -> false

  let is_event t =
    1 = List.length t.enums &&
    match (List.hd @@ List.rev @@ t.path) with
    | `Field "event" -> true
    | _ -> false


end


let rec strip_path ~path =
  match path with
  | `Field "allOf" :: `Index _ :: rest
  | `Field "oneOf" :: `Index _ :: rest
  | `Field "anyOf" :: `Index _ :: rest
  | `Field "not" :: `Index _ :: rest
  | `Field "properties" :: rest ->
    strip_path ~path:rest
  | `Field fname :: rest -> fname :: (strip_path ~path:rest)
  | [] -> []
  | _ -> assert false

let dirty_name ~path =
  Some (List.hd @@ List.rev @@ strip_path ~path)

let is_special_definition ~path =
  let pth = Array.of_list path in
  let n = Array.length pth in
  if n = 2 && pth.(0) = `Field "definitions" then
    match pth.(1) with
    | `Field "ProtocolMessage" | `Field "Request" | `Field "Event" | `Field "Response" -> true
    | _ -> false
  else
    false


type t =
  | Object of Obj_spec.t
  | EmptyObject
  | Enum of Enum_spec.t
  | Field of Field_spec.t
  | Request of Obj_spec.t
  | Response of Obj_spec.t
  | Event of Obj_spec.t
  [@@deriving show]

let make ~dirty_name ~path ?dirty_names () =
  match dirty_names with
  | Some dirty_names -> Enum (Enum_spec.of_path ~dirty_name ~path ~dirty_names ())
  | None ->
      let specs = Obj_spec.of_path ~dirty_name ~path () in Object specs

let to_string = show
