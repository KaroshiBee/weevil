open Protocol

let file_arg = "FILE"

(* NOTE type unparsing_mode = Optimized | Readable | Optimized_legacy, could we phantom this into the logger type? *)
module Interpreter_cfg = struct
  type input = string
  let to_string_input i = i
  let from_string_input s = Environment.Error_monad.ok s

  type output = string
  let to_string_output o = o
  let from_string_output s = Environment.Error_monad.ok s

  let unparsing_mode = Script_ir_translator.Readable
end

module Interpreter = Mdb_traced_interpreter.T (Interpreter_cfg)
module Stepper = Mdb_stepper2.T (Interpreter)

let protocol_str = "PtKathmankSpLLDALzWw7CGD2j2MtyveTwboEYokqUCP4a1LxMg"
let base_dir = "/tmp/.weevil"

let make_logger = Interpreter.trace_logger ~in_channel:stdin ~out_channel:stdout

let process headless script_filename_opt =
  let stepper =
    let open Lwt_result_syntax in
    match script_filename_opt with
    | Some script_filename ->
      let* stepper = Stepper.init ~protocol_str ~base_dir () in
      let* script = Stepper.typecheck ~script_filename stepper in
      Stepper.step ~make_logger ~script stepper
    | None ->
      Lwt.return @@ error_with_exn @@ Invalid_argument "expected a script filename"
  in

  let post_process res =
    let open Lwt_syntax in
    let* stepper_result = res in
    match stepper_result with
    | Ok _ -> (* TODO dont ignore OK output *) Lwt.return @@ `Ok ()
    | Error errs as e ->
      let ss =
        Format.asprintf "Stepper error - %a" pp_print_trace errs
      in
      let () =
        if headless then
          let enc = result_encoding Data_encoding.unit in
          let err_msg = Data_encoding.Json.(construct enc e |> to_string) |> Dapper.Dap.Header.wrap in
          Printf.fprintf stderr "%s" err_msg;
        else ()
      in
      (* if in headless mode then dont show --help if the cli has errors *)
      Lwt.return @@ `Error (not headless, ss)
  in

  Lwt_main.run @@ post_process stepper


module Tm = struct
  open Cmdliner
  let headless_arg =
    let doc =
      "Run the tool in headless mode, \
       this means that --help is not shown on error and \
       any errors are returned as JSON"
    in
    Arg.(
      value & flag & info ["h"; "headless"] ~doc
    )

  let contract_file_arg =
    let doc =
      "The Michelson contract filename that the weevil stepper will execute (required)."
    in
    (* NOTE we use value here rather than required because
       we want headless mode to not show --help when this arg is not given *)
    Arg.(
      value & pos 0 (some string) None & info [] ~doc ~docv:file_arg
    )

  let term =
    Term.(
      ret
        (const process $ headless_arg $ contract_file_arg)
    )

end

module Manpage = struct
  let command_description =
    "Run the Weevil Michelson stepper for the backend service"

  let description = [`S "DESCRIPTION"; `P command_description]

  let man = description

  let info = Cmdliner.Cmd.info ~doc:command_description ~man "stepper"
end

let cmd = Cmdliner.Cmd.v Manpage.info Tm.term
