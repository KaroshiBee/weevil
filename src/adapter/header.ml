
let _HEADER_FIELD = "Content-Length: "
let _HEADER_TOKEN = "\r\n\r\n"

let _replace input output =
  Str.global_replace (Str.regexp_string input) output

let _contains s1 s2 =
    let re = Str.regexp_string s2 in
    try
      ignore (Str.search_forward re s1 0);
      true
    with Not_found -> false

let wrap ?(add_header=true) msg =
  let s = msg |> _replace "\n" ""
  in
  if add_header then
    let n = String.length s in
    Printf.sprintf "%s%d%s%s" _HEADER_FIELD n _HEADER_TOKEN s
  else
    s

let content_length =
  let rgx = Str.regexp_string _HEADER_FIELD in
  fun msg ->
    if _contains msg _HEADER_FIELD then
      match Str.split rgx msg with
      | [n] ->
        let i = int_of_string n in
        Some i
      | _ -> None
    else None
