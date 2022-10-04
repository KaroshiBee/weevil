module Q = Json_query

let _unweird_name ?(capitalize=false) name =
  let n = match (String.lowercase_ascii name) with
  | "type" | "module" | "lazy" -> Printf.sprintf "%s_" name
  | "null" -> "empty_"
  | _ -> name
  in
  n
  |> Stringext.replace_all ~pattern:" " ~with_:"_"
  |> (fun s -> if capitalize then String.capitalize_ascii s else s)


let root_path ~(path:Q.path) =
  match path with
  | (`Field "definitions") :: (`Field f) :: _ -> [(`Field "definitions"); (`Field f)]
  | _ -> []


module Field_spec = struct
  type t = {
    safe_name:string; (* for being a record name in ocaml *)
    dirty_name: string; (* what it gets written back to json field as *)
    path: Q.path; [@printer Q.print_path_as_json_pointer]
    module_name: string;
    type_:string; (* the ocaml type *)
    enc_: string; (* the encoding function name *)
    required:bool;
  } [@@deriving show]

  let make ~path ~dirty_name ~required ?(module_name="") ?(type_="") ?(enc_="") () =
    let safe_name = _unweird_name dirty_name in
    {
      safe_name;
      dirty_name;
      path;
      module_name;
      type_;
      enc_;
      required;
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
    let safe_name = _unweird_name dirty_name in
    {safe_name; dirty_name; path; fields; is_cyclic}

  let is_big t =
    List.length t.fields > 10

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
  } [@@deriving show]

  let of_path ~dirty_name ~path ?(dirty_names=[]) () =
    let safe_name = _unweird_name dirty_name in
    let enums =
      dirty_names
      |> List.map (fun dirty_name ->
          let safe_name = _unweird_name ~capitalize:true dirty_name in
          {safe_name; dirty_name}
        )
    in
    {safe_name; dirty_name; path; enums}

  let set_enums t ~dirty_names =
    let enums =
      dirty_names
      |> List.map (fun dirty_name ->
          let safe_name = _unweird_name ~capitalize:true dirty_name in
          {safe_name; dirty_name}
        )
    in
    {t with enums}

  let append_enum t ~dirty_name =
    let safe_name = _unweird_name ~capitalize:true dirty_name in
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

module Req_spec = struct
  type t = {
    safe_name:string; (* for being a record name in ocaml *)
    dirty_name: string; (* what it gets written back to json field as *)
    path: Q.path; [@printer Q.print_path_as_json_pointer]
    command: string;
    args: Obj_spec.t option;
  } [@@deriving show]

  exception Not_request of string

  let of_path_exn ~dirty_name ~path ?args () =
    let safe_name = _unweird_name dirty_name in
    match path with
    | (`Field "definitions") :: (`Field v) :: [] ->
          let command =
            match Stringext.cut v ~on:"Request" with
            | Some (enum_str, "") -> enum_str
            | _ -> raise @@ Not_request (Q.json_pointer_of_path path)
          in
          {safe_name; dirty_name; path; command; args}
    | _ -> raise @@ Not_request (Q.json_pointer_of_path path)

  let set_args t ~args =
    {t with args=(Some args)}

  let is_arguments_for t ~(obj_specs:Obj_spec.t) =
    let root_path = root_path ~path:obj_specs.path in
    let is_same = String.equal (Q.json_pointer_of_path t.path) (Q.json_pointer_of_path root_path) in
    (* NOTE arguments field should be at /definitions/XXXRequest/allOf/1/properties/arguments *)
    is_same && 6 = List.length obj_specs.path &&
    match (List.hd @@ List.rev @@ obj_specs.path) with
    | `Field "arguments" -> true
    | _ -> false


end


module Resp_spec = struct
  type t = {
    safe_name:string; (* for being a record name in ocaml *)
    dirty_name: string; (* what it gets written back to json field as *)
    path: Q.path; [@printer Q.print_path_as_json_pointer]
    command: string;
    body: Obj_spec.t option;
    message: Obj_spec.t option;
  } [@@deriving show]

  exception Not_response of string

  let of_path_exn ~dirty_name ~path ?body ?message () =
    let safe_name = _unweird_name dirty_name in
    match path with
    | (`Field "definitions") :: (`Field v) :: [] ->
          let command =
            match Stringext.cut v ~on:"Response" with
            | Some (enum_str, "") -> enum_str
            | _ -> raise @@ Not_response (Q.json_pointer_of_path path)
          in
          {safe_name; dirty_name; path; command; body; message}
    | _ -> raise @@ Not_response (Q.json_pointer_of_path path)

  let set_body t ~body =
    {t with body=(Some body)}

  let is_body_for t ~(obj_specs:Obj_spec.t) =
    let root_path = root_path ~path:obj_specs.path in
    let is_same = String.equal (Q.json_pointer_of_path t.path) (Q.json_pointer_of_path root_path) in
    (* NOTE body field should be at /definitions/XXXResponse/allOf/1/properties/body *)
    is_same && 6 = List.length obj_specs.path &&
    match (List.hd @@ List.rev @@ obj_specs.path) with
    | `Field "body" -> true
    | _ -> false

  let set_message t ~message =
    {t with message=(Some message)}

  let is_message_for t ~(obj_specs:Obj_spec.t) =
    let root_path = root_path ~path:obj_specs.path in
    let is_same = String.equal (Q.json_pointer_of_path t.path) (Q.json_pointer_of_path root_path) in
    (* NOTE message field should be at /definitions/XXXMessage/allOf/1/properties/message *)
    is_same && 6 = List.length obj_specs.path &&
    match (List.hd @@ List.rev @@ obj_specs.path) with
    | `Field "message" -> true
    | _ -> false

end


module Event_spec = struct
  type t = {
    safe_name:string; (* for being a record name in ocaml *)
    dirty_name: string; (* what it gets written back to json field as *)
    path: Q.path; [@printer Q.print_path_as_json_pointer]
    event: string;
    body: Obj_spec.t option;
  } [@@deriving show]

  exception Not_event of string

  let of_path_exn ~dirty_name ~path ?body () =
    let safe_name = _unweird_name dirty_name in
    match path with
    | (`Field "definitions") :: (`Field v) :: [] ->
          let event =
            match Stringext.cut v ~on:"Event" with
            | Some (enum_str, "") -> enum_str
            | _ -> raise @@ Not_event (Q.json_pointer_of_path path)
          in
          {safe_name; dirty_name; path; event; body}
    | _ -> raise @@ Not_event (Q.json_pointer_of_path path)

  let set_body t ~body =
    {t with body=(Some body)}

  let is_body_for t ~(obj_specs:Obj_spec.t) =
    let root_path = root_path ~path:obj_specs.path in
    let is_same = String.equal (Q.json_pointer_of_path t.path) (Q.json_pointer_of_path root_path) in
    (* NOTE body field should be at /definitions/XXXEvent/allOf/1/properties/body *)
    is_same && 6 = List.length obj_specs.path &&
    match (List.hd @@ List.rev @@ obj_specs.path) with
    | `Field "body" -> true
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
  | Enum of Enum_spec.t
  | Field of Field_spec.t
  [@@deriving show]

let make ~dirty_name ~path ?dirty_names () =
  match dirty_names with
  | Some dirty_names -> Enum (Enum_spec.of_path ~dirty_name ~path ~dirty_names ())
  | None ->
      let specs = Obj_spec.of_path ~dirty_name ~path () in Object specs

let to_string = show
