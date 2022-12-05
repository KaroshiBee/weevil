
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

let get_content_length =
  let rgx = Str.regexp_string _HEADER_FIELD in
  fun msg ->
    if _contains msg _HEADER_FIELD then
      match Str.split rgx msg with
      | [_; n]
      | [n] ->
        let i = int_of_string n in
        Some i
      | _ -> None
    else None


let rec content_length_message_handler
    ~name
    ~handle_message
    ~content_length
    ic oc =

  match content_length with
  | None -> (
      let%lwt () = Logs_lwt.info (fun m -> m "[%s] waiting for content-length messages" name) in
      match%lwt Lwt_io.read_line_opt ic with
      | Some msg ->
          let%lwt () = Logs_lwt.info (fun m -> m "[%s] got messsage '%s'" name msg) in
          (* if theres content length info in there, strip it out *)
          let content_length = get_content_length msg in
          let%lwt () =
            match content_length with
            | Some i ->
              Logs_lwt.info (fun m -> m "[%s] got content length %d in '%s'" name i msg)
            | None ->
              Logs_lwt.info (fun m -> m "[%s] no content length in '%s' yet" name msg)
          in
          content_length_message_handler
            ~name
            ~handle_message
            ~content_length
            ic
            oc
      | None -> Logs_lwt.info (fun m -> m "[%s] connection closed" name))
  | Some count ->
      let%lwt () = Logs_lwt.info (fun m ->
          m "[%s] got content-length message with length %d" name count)
      in
      (* \r\n throw away *)
      let%lwt header_break = Lwt_io.read ~count:2 ic in
      let%lwt () = Logs_lwt.info (fun m ->
          m "[%s] got content-length message with length %d and header_break '%s'" name count header_break)
      in
      let () = assert (header_break = "\r\n") in
      let%lwt msg = Lwt_io.read ~count ic in
      let%lwt () = Logs_lwt.info (fun m ->
          m "[%s] got content-length message with message '%s'" name msg)
      in
      let%lwt _ = handle_message msg ic oc in
      let content_length = None in
      content_length_message_handler ~name ~handle_message ~content_length ic oc

let content_length = get_content_length
