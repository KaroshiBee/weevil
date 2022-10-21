module Js = Data_encoding.Json
module Q = Json_query

let _HEADER_FIELD = "Content-Length: "
let _HEADER_TOKEN = "\r\n\r\n"

type t = Js.json

exception Wrong_encoder of string

let _replace input output =
  Str.global_replace (Str.regexp_string input) output

let from_string = Js.from_string

let to_string t =
  let s = t
          |> Js.to_string
          |> _replace "\n" ""
  in
  let n = String.length s in
  Printf.sprintf "%s%d%s%s" _HEADER_FIELD n _HEADER_TOKEN s

let construct enc i =
  try
    Js.construct enc i
  with _ as e ->
    let s = Printf.sprintf "cannnot construct: %s" (Printexc.to_string e) in
    raise @@ Wrong_encoder s

let destruct enc i =
  try
    Js.destruct enc i
  with
  | Js.Cannot_destruct (pth, Js.Unexpected(typestr, value)) ->
    let s = Printf.sprintf "cannot destruct @ %s, expected '%s':%s"
        (Q.json_pointer_of_path pth) value typestr in
    raise @@ Wrong_encoder s

  | Data_encoding__.Binary_error_types.Invariant_guard err ->
    let s = Printf.sprintf "cannnot destruct: %s" err in
    raise @@ Wrong_encoder s

  | _ as e ->
    let s = Printf.sprintf "cannnot destruct: %s" (Printexc.to_string e) in
    raise @@ Wrong_encoder s
