module Js = Data_encoding.Json
module Q = Json_query


type t = Js.json
type wrong_encoder_details = [
  | `Cannot_construct
  | `Cannot_destruct
  | `Cannot_destruct_wrong_fields
]
exception Wrong_encoder of (string * wrong_encoder_details)


let from_string = Js.from_string

(* NOTE dont Dap_header.wrap yet *)
let to_string t = Js.to_string t

let construct enc i =
  try
    Js.construct enc i
  with _ as e ->
    let s = Printf.sprintf "cannnot construct: %s" (Printexc.to_string e) in
    raise @@ Wrong_encoder (s, `Cannot_construct)

let destruct enc i =
  try
    Js.destruct enc i
  with
  | Js.Cannot_destruct (pth, Js.Unexpected(typestr, value)) ->
    let s = Printf.sprintf "cannot destruct @ %s, expected '%s':%s"
        (Q.json_pointer_of_path pth) value typestr in
    raise @@ Wrong_encoder (s, `Cannot_destruct_wrong_fields)

  | Data_encoding__.Binary_error_types.Invariant_guard err ->
    let s = Printf.sprintf "cannnot destruct: %s" err in
    raise @@ Wrong_encoder (s, `Cannot_destruct)

  | _ as e ->
    let s = Printf.sprintf "cannnot destruct: %s" (Printexc.to_string e) in
    raise @@ Wrong_encoder (s, `Cannot_destruct)
