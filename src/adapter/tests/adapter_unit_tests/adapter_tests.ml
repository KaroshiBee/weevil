(* A module with functions to test *)
module To_test = struct
  let lowercase = String.lowercase_ascii
  let capitalize = String.capitalize_ascii
  let str_concat = String.concat ""
  let list_concat = List.append
end

(* The tests *)
let test_lowercase () =
  Alcotest.(check string) "same string" "hello!" (To_test.lowercase "hELLO!")

let test_capitalize () =
  Alcotest.(check string) "same string" "World." (To_test.capitalize "world.")

let test_str_concat () =
  Alcotest.(check string) "same string" "foobar" (To_test.str_concat ["foo"; "bar"])

let test_list_concat () =
  Alcotest.(check (list int)) "same lists" [1; 2; 3] (To_test.list_concat [1] [2; 3])

(* Run it *)
let () =
  let open Alcotest in
  run "Utils" [
      "string-case", [
          test_case "Lower case"     `Quick test_lowercase;
          test_case "Capitalization" `Quick test_capitalize;
        ];
      "string-concat", [ test_case "String mashing" `Quick test_str_concat  ];
      "list-concat",   [ test_case "List mashing"   `Slow  test_list_concat ];
    ]

(* include Test_utils.Include *)
(* module Js_msg = Dapper.Dap_js_msg *)

(* let config = Dapper.Dap_config.make () *)

(* let%expect_test "Check cancel handler" = *)
(*   let open Cancel in *)
(*   let s = {| { "seq": 10, "type": "request", "command": "cancel", "arguments": { "requestId": 1 } } |} in *)
(*   let backend = Cancel.make_empty in *)
(*   let%lwt o = string_to_input s |> handle backend config in *)
(*   let%lwt ss = output_to_string o in *)
(*   Printf.printf "%s" @@ Result.get_ok ss; *)

(*   let%lwt _ = [%expect {| *)
(*     Content-Length: 103 *)
(*      *)
(*     { "seq": 11, "type": "response", "request_seq": 10, "success": true,  "command": "cancel", "body": {} } |}] in *)


(*   (\* test a bad input - NOTE the wrong command *\) *)
(*   let s = {| { "seq": 10, "type": "request", "command": "initialize", "arguments": { "requestId": 1 } } |} in *)
(*   let ss = *)
(*     try *)
(*       let _ = string_to_input s in "" *)
(*     with *)
(*     | Js_msg.Wrong_encoder err -> err *)
(*   in *)
(*   Printf.printf "%s" ss; *)

(*   [%expect {| cannnot destruct: expected 'cancel', got 'initialize' |}] *)

(* let%expect_test "Check initialize handler" = *)
(*   let open Initialize in *)
(*   let s = {| { "seq": 10, "type": "request", "command": "initialize", "arguments": { "adapterID": "weevil", "clientID":"1" } } |} in *)
(*   let backend = Initialize.make_empty in *)
(*   let%lwt o = string_to_input s |> handle backend config in *)
(*   let%lwt ss = output_to_string o in *)
(*   Printf.printf "%s" @@ Result.get_ok ss; *)

(*   let%lwt _ = *)
(*     [%expect {| *)
(*     Content-Length: 107 *)
(*      *)
(*     { "seq": 11, "type": "response", "request_seq": 10, "success": true,  "command": "initialize", "body": {} }Content-Length: 66 *)
(*      *)
(*     { "seq": 12, "type": "event", "event": "initialized", "body": {} } |}] in *)

(*   (\* test a bad input - NOTE the clientId not clientID *\) *)
(*   let s = {| { "seq": 10, "type": "request", "command": "initialize", "arguments": { "adapterID": "weevil", "clientId":"1" } } |} in *)
(*   let ss = *)
(*     try *)
(*       let _ = string_to_input s in "" *)
(*     with *)
(*     | Js_msg.Wrong_encoder err -> err *)
(*   in *)
(*   Printf.printf "%s" ss; *)

(*   [%expect {| *)
(*     cannnot destruct: Json_encoding.Unexpected_field("clientId") |}] *)
