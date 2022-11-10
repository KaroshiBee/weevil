module Dap = Dapper.Dap

module type HANDLER = Types.String_handler_intf

type t = {handlers : (string, (module HANDLER)) Hashtbl.t}

let make =
  {
    handlers =
      [
        (* "cancel", (module Cancel : HANDLER); *)
        (* "initialize", (module Initialize : HANDLER); *)
        (* "configurationDone", (module Configuration : HANDLER); *)
        ("launch", (module Launch : HANDLER));
        ("attach", (module Attach : HANDLER));
        (* "next", (module Next : HANDLER); *)
        (* "restart", (module Restart : HANDLER); *)
        (* "disconnect", (module Disconnect : HANDLER); *)
        (* "terminate", (module Terminate : HANDLER); *)
      ]
      |> List.to_seq |> Hashtbl.of_seq;
  }

let _deduplicate_stable xs =
  (* de-dups the string list but keeps it in order *)
  let module SSet = Set.Make (String) in
  let (_, ys) =
    let init = (SSet.empty, []) in
    List.fold_left
      (fun (sset, out) -> function
        | x when SSet.mem x sset -> (sset, out)
        | _ as x ->
            let sset = SSet.add x sset in
            let out = x :: out in
            (sset, out))
      init
      xs
  in
  List.rev ys

let%expect_test "check deduplicate_stable" =
  let xs = [] in
  let s = _deduplicate_stable xs |> String.concat "," in
  Printf.printf "%s" s;
  [%expect {||}];

  let xs = [555;111;222;222;333;111;444] |> List.map string_of_int in
  let s = _deduplicate_stable xs |> String.concat "," in
  Printf.printf "%s" s;
  [%expect {| 555,111,222,333,444 |}]


let _find_handler ~config message = function
  | None -> Lwt.return_none
  | Some h -> (
      let module H = (val h : HANDLER) in
      let h = H.make () in
      let handlers = H.handlers ~config h in
      let init = Lwt.return (message, []) in
      try%lwt
        let%lwt (_, output) =
          List.fold_left
            (fun acc f ->
              let%lwt (inp, outp) = acc in
              let%lwt o = f inp in
              Lwt.return (o, o :: outp))
            init
            handlers
        in
        Lwt.return_some output
      with Dap.Wrong_encoder _ -> Lwt.return_none)

let handle_exn t config message =
  let%lwt output =
    let cmds =
      [
        "cancel";
        "initialize";
        "configurationDone";
        "launch";
        "attach";
        "next";
        "restart";
        "disconnect";
        "terminate";
      ]
    in
    (* First one that doesnt raise Wrong_encoder is what we want *)
    cmds
    |> Lwt_list.filter_map_p (fun cmd ->
           let h = Hashtbl.find_opt t.handlers cmd in
           _find_handler ~config message h)
  in

  let ret =
    match output with
    | [] ->
        Printf.sprintf "[DAP] Cannot handle message: '%s'" message
        |> Result.error
    | output :: _ -> Result.ok @@ _deduplicate_stable output
  in
  Lwt.return ret
