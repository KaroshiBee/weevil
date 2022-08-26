module Q = Json_query

let is_special_definition ~path =
  let pth = Array.of_list path in
  let n = Array.length pth in
  if n = 2 && pth.(0) = `Field "definitions" then
    match pth.(1) with
    | `Field "ProtocolMessage" | `Field "Request" | `Field "Event" | `Field "Response" -> true
    | _ -> false
  else
    false

module Req = struct
  type t = {
    path: Q.path;
    command: string;
    args: string option;
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

end


module Resp = struct
  type t = {
    path: Q.path;
    command: string;
    body: string option;
    message: string option;
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

  let set_message t ~message =
    {t with message=(Some message)}

end


module Event = struct
  type t = {
    path: Q.path;
    event: string;
    body: string option;
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

end


let _unweird_name ?(capitalize=false) name =
  let n = match (String.lowercase_ascii name) with
  | "type" | "module" | "lazy" -> Printf.sprintf "%s_" name
  | "null" -> "empty_"
  | _ -> name
  in
  n
  |> Stringext.replace_all ~pattern:" " ~with_:"_"
  |> (fun s -> if capitalize then String.capitalize_ascii s else s)


module Obj = struct

  type field = {
    safe_name:string;
    dirty_name: string;
    type_:string;
    enc_: string;
    required:bool;
  }

  type t = {
    path: Q.path;
    fields: field list;
    is_cyclic: bool;
  }

  let of_path ~path ?(fields=[]) ?(is_cyclic=false) () =
    {path; fields; is_cyclic}

  let root_path t =
    match t.path with
    | (`Field "definitions") :: (`Field f) :: _ -> [(`Field "definitions"); (`Field f)]
    | _ -> []

  let is_big t =
    List.length t.fields > 10

end


module Enum = struct

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

end
