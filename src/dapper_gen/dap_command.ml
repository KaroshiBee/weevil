let _DEFAULT_SCHEMA = "./schema/debugAdapterProtocol-1.56.X.json"
let _DEFAULT_OUTPUT = "src/dapper_gen/dap_encoders.ml"

let process json_schema_arg output_file_arg =
  let p ?(json_schema_arg=_DEFAULT_SCHEMA) ?(output_file_arg=_DEFAULT_OUTPUT) () =
    let schema_js = Ezjsonm.from_channel @@ open_in json_schema_arg in
    let dfs = Dap_dfs.Dfs.make ~schema_js in
    let txt = Dap_render.render dfs in
    let o = open_out output_file_arg in
    let res =
      try
        `Ok (Printf.fprintf o "%s" txt)
      with err ->
        let e = Printexc.to_string err in
        Logs.err (fun m -> m "%s" e);
        `Error (true, e)
    in
    close_out o;
    res
  in
  p ?json_schema_arg ?output_file_arg ()



module Term = struct
  let json_schema_arg =
    let open Cmdliner in
    let doc =
      Format.sprintf
        "The DAP JSON schema filename that the weevil will use to auto-generate DAP encoders.  \
        If not given then defaults to <%s>" _DEFAULT_SCHEMA
    in
    Arg.(
      value & pos 0 (some string) None & info [] ~doc ~docv:"SCHEMA"
    )

  let output_file_arg =
    let open Cmdliner in
    let doc =
      Format.sprintf
        "The output filename that the weevil will write auto-generate DAP encoders to.  \
        If not given then defaults to <%s>" _DEFAULT_OUTPUT
    in
    Arg.(
      value & pos 1 (some string) None & info [] ~doc ~docv:"FILE"
    )

  let term =
    Cmdliner.Term.(
      ret
        (const process $ json_schema_arg $ output_file_arg)
    )

end

module Manpage = struct
  let command_description =
    "Run the Weevil DAP autogenerator to auto-generate DAP encoder code"

  let description = [`S "DESCRIPTION"; `P command_description]

  let man = description

  let info = Cmdliner.Cmd.info ~doc:command_description ~man "dap-gen"
end

let cmd = Cmdliner.Cmd.v Manpage.info Term.term
