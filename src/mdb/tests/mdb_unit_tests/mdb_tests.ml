open Protocol
open Environment
open Alpha_context
open Environment.Error_monad

module TI = Mdb.Mdb_traced_interpreter
module Tbl = Mdb.Mdb_log_records

module TestCfg = struct

  type t =
    | TestLog of int

  let make_log _ctxt _loc _stack _stack_ty =
    TestLog 1

  let unparsing_mode = Script_ir_translator.Readable

  let unparse_stack = function
    | TestLog _i -> Lwt_result_syntax.return []

  let get_loc _ = 100

  (* cant make Gas.t directly *)
  let get_gas _ = Data_encoding.Json.(destruct Gas.encoding (`String "unaccounted"))

end

module TestInterp = TI.T (TestCfg)

(* The tests *)
let test_get_execution_trace_updates () =
  let tbl = Tbl.make () in
  Tbl.add_new tbl 3 @@ TestCfg.TestLog 3;
  Tbl.add_old tbl 2 @@ TestCfg.TestLog 2;
  Tbl.add_new tbl 1 @@ TestCfg.TestLog 1;

  (* check before *)
  let is_in = Tbl.mem tbl in
  let all_in = is_in 1 && is_in 2 && is_in 3 in
  let () =
    Alcotest.(check bool) "contains locs 1,2,3 before" all_in true
  in

  let () =
    match Tbl.to_list tbl with
    | [TestCfg.TestLog 1; TestCfg.TestLog 3;] -> ()
    | xs ->
        xs |> List.iter (function TestCfg.TestLog x -> Printf.printf "%d" x);
        failwith "should return sorted list by loc of only new ones"
  in

  let () = Tbl.new_to_old_inplace ~keep_old:false tbl in
  let _2_not_in = not @@ is_in 2 in
  let () =
    Alcotest.(check bool) "no longer contains locs 2 after" _2_not_in true
  in

  match Tbl.to_list tbl with
  | [] -> ()
  | _ -> failwith "should not have any new entries left"

let () =
  let open Alcotest in
  run "Traced interpreter" [
      "log records table", [
          test_case "Only new ones" `Quick test_get_execution_trace_updates;
        ];
    ]
