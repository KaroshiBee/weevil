let _DEFAULT_SCHEMA = "./schema/debugAdapterProtocol-1.56.X.json"
let _DEFAULT_OUTPUT = "test"
let _DEFAULT_WHAT = Dap_render.Messages

(* NOTE type unparsing_mode = Optimized | Readable | Optimized_legacy, could we phantom this into the logger type? *)
let process what_arg json_schema_arg output_file_arg () =
  let p ?what_arg ?(json_schema_arg=_DEFAULT_SCHEMA) ?(output_file_arg=_DEFAULT_OUTPUT) () =
    let schema_js = Ezjsonm.from_channel @@ open_in json_schema_arg in
    let dfs = Dap_dfs.Dfs.make ~schema_js in
    let what_args = Dap_render.(
        match what_arg with
        | Some "commands" -> [Commands ML; Commands MLI]
        | Some "events" -> [Events ML; Events MLI]
        | _ -> [Messages]
      ) in
    let results = what_args |> List.map (fun w ->
        let txt = Dap_render.(render dfs w) in
        let filename = Dap_render.(
            match w with
            | Messages
            | Events ML
            | Commands ML -> output_file_arg^".ml"
            | Events MLI
            | Commands MLI -> output_file_arg^".mli"
          ) in
        let o = open_out filename in
        let res =
          try
            Result.ok (Printf.fprintf o "%s" txt)
          with err ->
            let e = Printexc.to_string err in
            Logs.err (fun m -> m "%s" e);
            Result.error (true, e)
        in
        close_out o;
        res
      ) in
    let res = match (results |> List.filter Result.is_error) with
      | [] -> `Ok ()
      | errors ->
        let err = errors |> List.map (function Result.Error (_, e) -> e | _ -> assert false) in
        `Error (true, String.concat "\n" err)
    in
    res

  in
  p ?what_arg ?json_schema_arg ?output_file_arg ()



module Term = struct
  let what_arg =
    let open Cmdliner in
    let doc =
        "What to auto-generate: Messages (ml file), Commands (ml/mli files), Events (ml/mli files).  \
        If not given then defaults to <Messages>"
    in
    Arg.(
      value & pos 0 (some string) None & info [] ~doc ~docv:"WHAT"
    )

  let json_schema_arg =
    let open Cmdliner in
    let doc =
      Format.sprintf
        "The DAP JSON schema filename that the weevil will use to auto-generate DAP encoders.  \
        If not given then defaults to <%s>" _DEFAULT_SCHEMA
    in
    Arg.(
      value & pos 1 (some string) None & info [] ~doc ~docv:"SCHEMA"
    )

  let output_file_arg =
    let open Cmdliner in
    let doc =
      Format.sprintf
        "The output filename that the weevil will write auto-generate DAP encoders to.  \
        If not given then defaults to <%s.ml/mli>" _DEFAULT_OUTPUT
    in
    Arg.(
      value & pos 2 (some string) None & info [] ~doc ~docv:"FILE"
    )

  let term setup_log =
    Cmdliner.Term.(
      ret
        (const process $ what_arg $ json_schema_arg $ output_file_arg $ setup_log)
    )

end

module Manpage = struct
  let command_description =
    "Run the Weevil DAP autogenerator to auto-generate DAP encoder code"

  let description = [`S "DESCRIPTION"; `P command_description]

  let man = description

  let info = Cmdliner.Cmd.info ~doc:command_description ~man "dap-gen"
end

let cmd setup_log = Cmdliner.Cmd.v Manpage.info @@ Term.term setup_log
