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
  type field = {
    safe_name:string; (* for being a record name in ocaml *)
    dirty_name: string; (* what it gets written back to json field as *)
    type_:string; (* the ocaml type *)
    enc_: string; (* the encoding function name *)
    required:bool;
  }

  type t = {
    path: Q.path;
    fields: field list;
    is_cyclic: bool;
  }

  let of_path ~path ?(fields=[]) ?(is_cyclic=false) () =
    {path; fields; is_cyclic}

  let is_big t =
    List.length t.fields > 10

end


module Enum_spec = struct

  type enum_val = {
    safe_name: string;
    dirty_name: string;
  }

  type t = {
    path: Q.path;
    enums: enum_val list;
  }

  let of_path ~path ?(dirty_names=[]) () =
    let enums =
      dirty_names
      |> List.map (fun dirty_name ->
          let safe_name = _unweird_name ~capitalize:true dirty_name in
          {safe_name; dirty_name}
        )
    in
    {path; enums}

  let set_enums t ~dirty_names =
    let enums =
      dirty_names
      |> List.map (fun dirty_name ->
          let safe_name = _unweird_name ~capitalize:true dirty_name in
          {safe_name; dirty_name}
        )
    in
    {t with enums}

  let append_enum  t ~dirty_name =
    let safe_name = _unweird_name ~capitalize:true dirty_name in
    {t with enums={safe_name; dirty_name}::t.enums}

  let append_enums  t ~enums =
    let dirty_names = List.concat [
          List.map (fun nm -> nm.dirty_name) enums;
          List.map (fun nm -> nm.dirty_name) t.enums
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

let is_special_definition ~path =
  let pth = Array.of_list path in
  let n = Array.length pth in
  if n = 2 && pth.(0) = `Field "definitions" then
    match pth.(1) with
    | `Field "ProtocolMessage" | `Field "Request" | `Field "Event" | `Field "Response" -> true
    | _ -> false
  else
    false

module Req_spec = struct
  type t = {
    path: Q.path;
    command: string;
    args: Obj_spec.t option;
  }

  exception Not_request of string

  let of_path_exn ~path ?args () =
    match path with
    | (`Field "definitions") :: (`Field v) :: [] ->
          let command =
            match Stringext.cut v ~on:"Request" with
            | Some (enum_str, "") -> enum_str
            | _ -> raise @@ Not_request (Q.json_pointer_of_path path)
          in
          {path; command; args}
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
    path: Q.path;
    command: string;
    body: Obj_spec.t option;
    message: Obj_spec.t option;
  }

  exception Not_response of string

  let of_path_exn ~path ?body ?message () =
    match path with
    | (`Field "definitions") :: (`Field v) :: [] ->
          let command =
            match Stringext.cut v ~on:"Response" with
            | Some (enum_str, "") -> enum_str
            | _ -> raise @@ Not_response (Q.json_pointer_of_path path)
          in
          {path; command; body; message}
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
    path: Q.path;
    event: string;
    body: Obj_spec.t option;
  }

  exception Not_event of string

  let of_path_exn ~path ?body () =
    match path with
    | (`Field "definitions") :: (`Field v) :: [] ->
          let event =
            match Stringext.cut v ~on:"Event" with
            | Some (enum_str, "") -> enum_str
            | _ -> raise @@ Not_event (Q.json_pointer_of_path path)
          in
          {path; event; body}
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


type t =
  | Request of Req_spec.t
  | Response of Resp_spec.t
  | Event of Event_spec.t
  | Object of Obj_spec.t
  | Enum of Enum_spec.t


let make ~path ?dirty_names () =
  match dirty_names with
  | Some dirty_names -> Enum (Enum_spec.of_path ~path ~dirty_names ())
  | None ->
    try let specs = Req_spec.of_path_exn ~path () in Request specs with Req_spec.Not_request _ ->
    try let specs = Resp_spec.of_path_exn ~path () in Response specs with Resp_spec.Not_response _ ->
    try let specs = Event_spec.of_path_exn ~path () in Event specs with Event_spec.Not_event _ ->
      let specs = Obj_spec.of_path ~path () in Object specs
