module Dap = Dapper.Dap

module type HANDLER = Types.String_handler_intf

type t = {
  handlers: (string, (module HANDLER)) Hashtbl.t;
}

let make = {
  handlers = [
    (* "cancel", (module Cancel : HANDLER); *)
    (* "initialize", (module Initialize : HANDLER); *)
    (* "configurationDone", (module Configuration : HANDLER); *)
    "launch", (module Launch : HANDLER);
    "attach", (module Attach : HANDLER);
    (* "next", (module Next : HANDLER); *)
    (* "restart", (module Restart : HANDLER); *)
    (* "disconnect", (module Disconnect : HANDLER); *)
    (* "terminate", (module Terminate : HANDLER); *)
  ] |> List.to_seq
    |> Hashtbl.of_seq;
}

type acc = (string * string list) Lwt.t
type f = string -> string Lwt.t
let fold_f = fun (acc:acc) (f:f) ->
  let%lwt inp, outp = acc in
  let%lwt o = f inp in
  Lwt.return (o, o :: outp)

let handle_exn t config message =
  let aux command =
    let h = Hashtbl.find t.handlers command in
    let module H = (val h : HANDLER) in
    let h = H.make () in
    let handlers = H.handlers ~config h in
    let init = Lwt.return (message, []) in
    try%lwt
      let%lwt (_, output) = List.fold_left fold_f init handlers in
      output
      |> Option.some
      |> Lwt.return
    with Dap.Wrong_encoder _ ->
      None |> Lwt.return
  in
  let%lwt output =
    let cmds = [
        "cancel";
        "initialize";
        "configurationDone";
        "launch";
        "attach";
        "next";
        "restart";
        "disconnect";
        "terminate";
      ] in
    Lwt_list.filter_map_p aux cmds
  in
  (* First one that doesnt error is what we want *)
  let ret =
    match output with
    (* TODO log this error instead *)
    | [] -> Printf.sprintf "[DAP] Cannot handle message: '%s'" message |> Result.error
    | output :: _ -> Result.ok output
  in
  Lwt.return ret
