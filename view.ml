open Notty
open Nottui
module W = Nottui_widgets
open Lwd_infix

module DRq = Dapper.Dap_request
module DRs = Dapper.Dap_response
module DEv = Dapper.Dap_event
module Db = Dapper.Dap_base
module Js = Data_encoding.Json


module Btn_stackFrames = struct

  type t = {
    old_state: Model.State.t;
    ic: Lwt_io.input_channel;
    oc: Lwt_io.output_channel;
  }

  let make old_state ic oc =
    {old_state; ic; oc}

  let render {old_state; ic; oc} oc_log =
    let btn_text = Printf.sprintf "[+]" in
    let btn_action () =
      let next_seq = Int64.succ old_state.current_seq in
      let args = DRq.StackTraceArguments.{threadId=Defaults._THE_THREAD_ID; startFrame=None; levels=None} in
      let req = new DRq.StackTraceRequest.cls next_seq args in
      let js = Js.construct DRq.StackTraceRequest.enc req |> Defaults.wrap_header in
      ignore (
        Lwt_io.write_line oc js >>= fun _ ->
        Lwt_io.read_line ic >>= fun _hdr ->
        Lwt_io.write_line oc_log _hdr >>= fun _ ->
        Lwt_io.read_line ic >>= fun _break ->
        Lwt_io.write_line oc_log "break" >>= fun _ ->
        Lwt_io.read_line ic >>= fun resp_str ->
        Lwt_io.write_line oc_log resp_str >>= fun _ ->
        Js.from_string resp_str |> Result.map (fun js ->
            let resp = Js.destruct DRs.StackTraceResponse.enc js in
            if resp#success then
              let current_seq = resp#seq in
              let stack_frames = resp#body.stackFrames in
              Lwd.set Model.state Model.State.{old_state with current_seq; stack_frames}
          ) |> Result.value ~default:() |> Lwt.return
      )
    in
    let btn =
      Ui.hcat [
        W.printf ~attr:A.empty "Stack frames ";
        W.button ~attr:A.(bg lightblue) btn_text btn_action;
      ]
    in
    let panel =
      List.fold_left (fun acc (sf:Db.StackFrame.t) ->
          let line = Printf.sprintf "%s: %d" sf.name (Int64.to_int sf.id) in
          let ui_line = W.string line in
          ui_line :: acc
        ) [] old_state.stack_frames
      |> List.rev |> Ui.vcat
    in
    [Ui.vcat [btn; panel]]


end


module Variable_frag = struct
  (* ie Gas/Stack etc *)

  type t = {
    var: Db.Variable_.t;
  }

  let make var =
    {var;}


  let render {var} =
    let line = Printf.sprintf "%s: %s" var.name var.value in
    [W.string line]

end


module Btn_scope = struct
  (* ie Arguments/Locals/Registers *)

  type t = {
    old_state: Model.State.t;
    ic: Lwt_io.input_channel;
    oc: Lwt_io.output_channel;
    title: string;
  }

  let make old_state ic oc title =
    {old_state; ic; oc; title}


  let render {old_state; ic; oc; title} oc_log =
    let btn_text = Printf.sprintf "[+]" in
    let btn_action () =
      let _ = old_state.scopes |> List.iter (fun (sc : Db.Scope.t) -> (
          let next_seq = Int64.succ old_state.current_seq in
          let args = DRq.VariablesArguments.{variablesReference=sc.variablesReference;} in
          let req = new DRq.VariablesRequest.cls next_seq args in
          let js = Js.construct DRq.VariablesRequest.enc req |> Defaults.wrap_header in
          ignore (
            Lwt_io.write_line oc js >>= fun _ ->
            Lwt_io.read_line ic >>= fun _hdr ->
            Lwt_io.write_line oc_log _hdr >>= fun _ ->
            Lwt_io.read_line ic >>= fun _break ->
            Lwt_io.write_line oc_log "break" >>= fun _ ->
            Lwt_io.read_line ic >>= fun resp_str ->
            Lwt_io.write_line oc_log resp_str >>= fun _ ->
            Lwt_io.write_line oc_log "HERE" >>= fun _ ->
            match Js.from_string resp_str |> Result.map (fun js ->
                let resp = Js.destruct DRs.VariablesResponse.enc js in
                if resp#success then
                  let current_seq = resp#seq in
                  let variables = resp#body.variables in
                  (* TODO append or set in Hashtbl for variables *)
                  Lwd.set Model.state Model.State.{old_state with current_seq; variables}
              ) with
            | Ok _ ->
              let s = Printf.sprintf "got %d variables" (List.length old_state.variables) in
              Lwt_io.write_line oc_log s
            | Error err ->
              let s = Printf.sprintf "ERROR: %s" err in
              Lwt_io.write_line oc_log s
              (* |> Result.value ~default:() |> Lwt.return *)
          )
        )) in ()
    in
    let btn =
      Ui.hcat [
        W.printf ~attr:A.empty "  %s" title;
        W.button ~attr:A.(bg lightblue) btn_text btn_action;
      ]
    in
    let ui_lines = List.fold_left (fun acc v ->
        let line = Variable_frag.make v in
        let ui_line = Variable_frag.render line in
        ui_line :: acc
      ) [] old_state.variables
    in
    let panel = ui_lines |> List.concat |> Ui.vcat in
    [Ui.vcat [btn; panel]]


end


module Btn_scopes = struct

  type t = {
    old_state: Model.State.t;
    ic: Lwt_io.input_channel;
    oc: Lwt_io.output_channel;
  }

  let make old_state ic oc =
    {old_state; ic; oc}

  let render {old_state; ic; oc} oc_log =
    let btn_text = Printf.sprintf "[+]" in
    let btn_action () =
      let next_seq = Int64.succ old_state.current_seq in
      let args = DRq.ScopesArguments.{frameId=Defaults._THE_FRAME_ID;} in
      let req = new DRq.ScopesRequest.cls next_seq args in
      let js = Js.construct DRq.ScopesRequest.enc req |> Defaults.wrap_header in
      ignore (
        Lwt_io.write_line oc js >>= fun _ ->
        Lwt_io.read_line ic >>= fun _hdr ->
        Lwt_io.write_line oc_log _hdr >>= fun _ ->
        Lwt_io.read_line ic >>= fun _break ->
        Lwt_io.write_line oc_log "break" >>= fun _ ->
        Lwt_io.read_line ic >>= fun resp_str ->
        Lwt_io.write_line oc_log resp_str >>= fun _ ->
        Js.from_string resp_str |> Result.map (fun js ->
            let resp = Js.destruct DRs.ScopesResponse.enc js in
            if resp#success then
              let current_seq = resp#seq in
              let scopes = resp#body.scopes in
              Lwd.set Model.state Model.State.{old_state with current_seq; scopes}
          ) |> Result.value ~default:() |> Lwt.return
      )
    in
    let btn =
      Ui.hcat [
        W.printf ~attr:A.empty "Scopes ";
        W.button ~attr:A.(bg lightblue) btn_text btn_action;
      ]
    in
    let panel =
      List.fold_left (fun acc (sc:Db.Scope.t) ->
          let line = Btn_scope.make old_state ic oc sc.name in
          let ui_line = Btn_scope.render line oc_log |> Ui.hcat in
          ui_line :: acc
        ) [] old_state.scopes
      |> List.rev |> Ui.vcat
    in
    [Ui.vcat [btn; W.string ""; panel]]


end


module Src_frag = struct

  type t = {
    highlighted: bool;
    src_line: string;
  }

  let make ?(highlighted=false) src_line =
    {highlighted; src_line}

  let render {highlighted; src_line} =
    let attr = if highlighted then A.(bg blue) else A.(bg black) in
    let src = if highlighted then Printf.sprintf "%s" src_line else src_line in
    [W.string ~attr src]

end

module Src_code = struct
  type t = {
    src_lines: Src_frag.t list
  }

  let make line_number lines =
    let src_lines = Src_frag.(
      lines
      |> List.mapi (fun i line ->
          let highlighted = i = line_number in
          make ~highlighted line)
    ) in
    {src_lines}

  let render {src_lines} =
    let ui_lines = List.fold_left (fun acc line ->
        let ui_line = Src_frag.render line in
        ui_line :: acc
      ) [] src_lines in
    ui_lines |> List.concat |> List.rev

end


module Btn_next = struct

  type t = {
    old_state: Model.State.t;
    max_lines:int;
    trace: Model.Weevil_record.t array;
    ic: Lwt_io.input_channel;
    oc: Lwt_io.output_channel;
  }

  let make old_state lines trace ic oc =
    let max_lines = List.length lines in
    {old_state; max_lines; trace; ic; oc}

  let render {old_state; max_lines; trace; ic; oc} oc_log =
    assert (0 = Array.length trace);
    (* let ln = min max_lines old_state.line_number |> max 0 in
     * let gas = "TODO gas" in (\* Model.Weevil_trace.get_safe trace ln |> Model.Weevil_record.get_gas in *\)
     * let loc = "TODO loc" in (\* Model.Weevil_trace.get_safe trace ln |> Model.Weevil_record.get_location in *\)
     * let exprs = "TODO expr" in (\* Model.Weevil_trace.get_safe trace ln |> Model.Weevil_record.get_expr_str in *\) *)

    (* button actions need to unpause the stepper thread *)
    let btn_text_down = Printf.sprintf "[-]" in
    let btn_action_down () =
      Lwd.set Model.state (Model.State.decr_line_number old_state max_lines)
    in
    let btn_text_up = Printf.sprintf "[+]" in
    let btn_action_up () =
      let next_seq = Int64.succ old_state.current_seq in
      let args = DRq.NextArguments.{threadId=Defaults._THE_THREAD_ID; singleThread=None; granularity=None} in
      let req = new DRq.NextRequest.cls next_seq args in
      let js = Js.construct DRq.NextRequest.enc req |> Defaults.wrap_header in
      ignore (
        Lwt_io.write_line oc js >>= fun _ ->
        Lwt_io.read_line ic >>= fun _hdr ->
        Lwt_io.write_line oc_log _hdr >>= fun _ ->
        Lwt_io.read_line ic >>= fun _break ->
        Lwt_io.write_line oc_log "break" >>= fun _ ->
        Lwt_io.read_line ic >>= fun _resp ->
        Lwt_io.write_line oc_log _resp >>= fun _ ->
        Lwt_io.read_line ic >>= fun _hdr ->
        Lwt_io.write_line oc_log _hdr >>= fun _ ->
        Lwt_io.read_line ic >>= fun _break ->
        Lwt_io.write_line oc_log "break" >>= fun _ ->
        Lwt_io.read_line ic >>= fun _stopped_ev ->
        Lwt_io.write_line oc_log _stopped_ev >>= fun _ ->
        (* TODO extract seq number and update state if success=true *)
        let current_seq = Int64.add next_seq 2L in
        Lwd.set Model.state (Model.State.incr_line_number old_state max_lines current_seq) |> Lwt.return
      )
    in
    [
      Ui.hcat [
        W.button ~attr:A.(bg lightred) btn_text_down btn_action_down;
        W.button ~attr:A.(bg lightgreen) btn_text_up btn_action_up;
        W.printf ~attr:A.empty "                                      ";
      ]
    ]

end


let split_out_code contract =
  let on = "code {" in
  match contract |> Stringext.cut ~on with
  | None -> []
  | Some (param_storage, src_code) ->
    let src_lines =
      src_code
      |> String.split_on_char ';'
      |> List.map (Stringext.replace_all ~pattern:"}}" ~with_:"")
      |> List.map (fun s -> "  " ^ s)
    in
    List.concat [ [(param_storage ^ on)];
                  src_lines;
                  ["}}"]
                ]

(* TODO
   pass contract code from command line and on to stepper

*)
let ui_main (ic:Lwt_io.input_channel) (oc:Lwt_io.output_channel) (oc_log:Lwt_io.output_channel) =
  let$ st = Lwd.get Model.state in
  let lines = split_out_code Defaults._THE_CONTRACT in
  let trace = [||] in
  let instr = W.printf "Mouse CLICK [-] or [+] to go back or forward resp.\nPress ALT-q to quit\n\n" in
  let step_btn = Btn_next.make st lines trace ic oc in
  let src_code_panel = Src_code.make st.line_number lines in
  let stackframe_btn = Btn_stackFrames.make st ic oc in
  let scopes_btn = Btn_scopes.make st ic oc in
  let code = [Btn_next.render step_btn oc_log; Src_code.render src_code_panel;] |> List.concat |> Ui.vcat in
  let state = [Btn_stackFrames.render stackframe_btn oc_log; [W.string ""]; Btn_scopes.render scopes_btn oc_log] |> List.concat |> Ui.vcat in
  let main = Ui.hcat [code; state] in
  Ui.vcat [instr; main]
