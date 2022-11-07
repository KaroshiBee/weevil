include Test_utils.Include
module Dap = Dapper.Dap

module StateMock = struct
  type t = unit
  let make_empty = ()

  let connect _ip _port = failwith "TODO"

  let process_none _t = failwith "MOCK"
  let set_process_none _t _process = failwith "MOCK"

  let ic _t = failwith "MOCK"
  let oc _t = failwith "TODO"
  let set_io _t _ic _oc = failwith "TODO"

  let launch_mode _t = failwith "TODO"
  let set_launch_mode _t _launch_mode = failwith "TODO"

end


module Attach = Attach.T (StateMock)

let%expect_test "Check sequencing etc for attach " =
  let config = Dap.Config.make () in
  let t = Attach.make () in
  match Attach.handlers ~config t with
  | f_resp :: f_ev :: _ ->
    let%lwt resp = f_resp "" in
    Printf.printf "%s" resp;

    let%lwt ev = f_ev resp in
    Printf.printf "%s" ev;
    Lwt.return_unit

  | _ -> failwith "error"

