module Dap = Dapper.Dap


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

  let apply_handler ~state message = function
    | None -> Lwt.return_none
    | Some h -> (
        let%lwt msgs =
          let module H = (val h : HANDLER_T) in
          let handlers = H.handlers ~state in
          let init = Lwt.return (Result.Ok message, []) in
          try%lwt
            let%lwt (final, output) =
              List.fold_left
                (fun acc f ->
                   (* NOTE the idea is to fold over the list of handlers that make up this action,
                      each handler f[i] is string -> (string, string) result.t Lwt.t
                      and the output from f[i] is passed as the input to f[i+1] as long as f[i] doesnt error,
                      if f[i] errors then all subsequent handlers are skipped,
                      at each stage either the non-errored output msg string or error msg string is added
                      to the list of returned messages *)
                   let%lwt (inp, outp) = acc in
                   match inp with
                   | Result.Ok inp ->
                     (* NOTE wrong encoder would be raised here *)
                     let%lwt o = f inp in
                     let%lwt msg_or_err =
                       match o with
                       | Result.Ok msg ->
                         let%lwt () = Logs_lwt.debug (fun m -> m "got input '%s', got reply '%s'" inp msg) in
                         Lwt.return msg
                       | Result.Error err ->
                         let%lwt () = Logs_lwt.err (fun m -> m "got input '%s', got error '%s'" inp err) in
                         Lwt.return err
                     in
                     Lwt.return (o, msg_or_err :: outp)
                   | Result.Error _ ->
                     Lwt.return (inp, outp)
                )
                init
                handlers
            in
            (* call on_success/on_error on final output, only care whether its in error or not *)
            let _ =
              final
              |> Result.map (fun _ -> H.on_success ~state)
              |> Result.map_error (fun _ -> H.on_error ~state)
            in
            Lwt.return_some @@ Result.Ok output
          with
          | Dap.Wrong_encoder (err, `Cannot_destruct_wrong_fields) ->
            (* NOTE we are trying to find the correct message handler by expected errors,
               `Cannot_destruct_wrong_fields implies we have the right handler but something else is wrong *)
            let%lwt () = Logs_lwt.err (fun m -> m "%s" err) in
            Lwt.return_some @@ Result.Error err
          | Dap.Wrong_encoder (_, _) ->
            (* NOTE these wrong encoder errors are ok *)
            Lwt.return_none
          | _ as e ->
            (* NOTE any other errors should be sent back too *)
            let err = Printexc.to_string e in
            let%lwt () = Logs_lwt.err (fun m -> m "%s" err) in
            Lwt.return_some @@ Result.Error err
        in
        Lwt.return @@
        Option.map (fun xs_ ->
            xs_ |> Result.map ( fun xs -> List.rev xs |> Utils.Helpers.deduplicate_stable )
          ) msgs
      )


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
          apply_handler ~state message h)
    in

    let%lwt ret =
      match output with
      | [] ->
        let%lwt () = Logs_lwt.err (fun m -> m "[DAP] Cannot handle message: '%s'" message) in
        Printf.sprintf "[DAP] Cannot handle message: '%s'" message
        |> Result.error |> Lwt.return
      | Result.Ok output :: [] -> Lwt.return @@ Result.ok output
      | Result.Error output :: [] -> Lwt.return @@ Result.Error output
      | _ :: rest ->
        let n = 1+List.length rest in
        let%lwt () = Logs_lwt.err (fun m -> m "[DAP] Got too many replies (%d) for msg '%s'" n message) in
        Lwt.return @@ Result.error @@ Printf.sprintf "Too many replies [%d] for message '%s'" n message

    in
    Lwt.return ret
end
