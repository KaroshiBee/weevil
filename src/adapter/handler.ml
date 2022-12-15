module Dap = Dapper.Dap

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


module T (S:Types.STATE_T) = struct

  module type HANDLER_T = Types.STRING_HANDLER_T with type state := S.t

  module Initialize = Initialize.T (S)
  module Configuration_done = Configuration_done.T (S)
  module Launch = Launch.T (S)
  module Attach = Attach.T (S)
  module Threads = Threads.T (S)
  module Stack_trace = Stack_trace.T (S)
  module Scopes = Scopes.T (S)
  module Variables = Variables.T (S)
  module Next = Next.T (S)
  module Terminate = Terminate.T (S)
  module Disconnect = Disconnect.T (S)


  type t = {handlers : (string, (module HANDLER_T)) Hashtbl.t;}

  let make =
    {
      handlers =
        [
          (* "cancel", (module Cancel : HANDLER_T with type state = S.t); *)
          "initialize", (module Initialize : HANDLER_T);
          "configurationDone", (module Configuration_done : HANDLER_T);
          "launch", (module Launch : HANDLER_T);
          "attach", (module Attach : HANDLER_T);
          "threads", (module Threads : HANDLER_T);
          "stack_trace", (module Stack_trace : HANDLER_T);
          "scopes", (module Scopes : HANDLER_T);
          "variables", (module Variables : HANDLER_T);
          "next", (module Next : HANDLER_T);
          (* "restart", (module Restart : HANDLER_T with type state = S.t); *)
          "terminate", (module Terminate : HANDLER_T);
          "disconnect", (module Disconnect : HANDLER_T);
        ]
        |> List.to_seq |> Hashtbl.of_seq;
    }

  let _find_handler ~state message = function
    | None -> Lwt.return_none
    | Some h -> (
        let module H = (val h : HANDLER_T) in
        let handlers = H.handlers ~state in
        let init = Lwt.return (message, []) in
        try%lwt
          let%lwt (_, output) =
            List.fold_left
              (fun acc f ->
                 let%lwt (inp, outp) = acc in
                 let%lwt o =
                   match%lwt f inp with
                   | Result.Ok msg -> Lwt.return msg
                   | Result.Error err -> Lwt.return err
                 in
                 Lwt.return (o, o :: outp))
              init
              handlers
          in
          let () = H.on_handled ~state in
          Lwt.return_some output
        with Dap.Wrong_encoder _ -> Lwt.return_none)

  let handle_exn t state message =
    let%lwt output =
      let cmds =
        [
          "cancel"; (* TODO *)
          "initialize";
          "configurationDone";
          "launch";
          "attach";
          "threads";
          "stack_trace";
          "scopes";
          "variables";
          "next";
          "restart"; (* TODO *)
          "terminate";
          "disconnect";
        ]
      in
      (* First one that doesnt raise Wrong_encoder is what we want *)
      cmds
      |> Lwt_list.filter_map_p (fun cmd ->
          let h = Hashtbl.find_opt t.handlers cmd in
          _find_handler ~state message h)
    in

    let ret =
      match output with
      | [] ->
        Printf.sprintf "[DAP] Cannot handle message: '%s'" message
        |> Result.error
      | output :: _ -> Result.ok @@ _deduplicate_stable output
    in
    Lwt.return ret
end
