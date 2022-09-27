open Dap_specs

let%expect_test "Check Req" =
  let path = [`Field "definitions"; `Field "CancelRequest"] in
  let x = Req_spec.of_path_exn ~dirty_name:"CancelRequest" ~path () in

  Printf.printf "%s" (Q.json_pointer_of_path x.path);
  [%expect {| /definitions/CancelRequest |}];

  Printf.printf "%s" x.command;
  [%expect {| Cancel |}];

  let path = Q.path_of_json_pointer "/definitions/ErrorResponse/allOf/0" in
  try
    let _ = Req_spec.of_path_exn ~dirty_name:"ErrorResponse" ~path () in ()
  with Req_spec.Not_request err ->
    Printf.printf "%s" err;
  [%expect {| /definitions/ErrorResponse/allOf/0 |}]



let%expect_test "Check Resp" =
  let path = [`Field "definitions"; `Field "CancelResponse"] in
  let x = Resp_spec.of_path_exn ~dirty_name:"CancelResponse" ~path () in

  Printf.printf "%s" (Q.json_pointer_of_path x.path);
  [%expect {| /definitions/CancelResponse |}];

  Printf.printf "%s" x.command;
  [%expect {| Cancel |}];

  let path = Q.path_of_json_pointer "/definitions/ErrorResponse/allOf/0" in
  try
    let _ = Resp_spec.of_path_exn ~dirty_name:"ErrorResponse"  ~path () in ()
  with Resp_spec.Not_response err ->
    Printf.printf "%s" err;
  [%expect {| /definitions/ErrorResponse/allOf/0 |}]


let%expect_test "Check Event" =
  let path = [`Field "definitions"; `Field "CancelEvent"] in
  let x = Event_spec.of_path_exn ~dirty_name:"CancelEvent" ~path () in

  Printf.printf "%s" (Q.json_pointer_of_path x.path);
  [%expect {| /definitions/CancelEvent |}];

  Printf.printf "%s" x.event;
  [%expect {| Cancel |}];

  let path = Q.path_of_json_pointer "/definitions/ErrorResponse/allOf/0" in
  try
    let _ = Event_spec.of_path_exn ~dirty_name:"ErrorResponse" ~path () in ()
  with Event_spec.Not_event err ->
    Printf.printf "%s" err;
  [%expect {| /definitions/ErrorResponse/allOf/0 |}]


let%expect_test "Check obj" =
  let path = Q.path_of_json_pointer "/definitions/ErrorResponse/allOf/0" in
  let x = Obj_spec.of_path ~dirty_name:"ErrorResponse" ~path () in

  Printf.printf "%s" (Q.json_pointer_of_path x.path);
  [%expect {| /definitions/ErrorResponse/allOf/0 |}];

  Printf.printf "%s" (Q.json_pointer_of_path (root_path ~path:x.path));
  [%expect {| /definitions/ErrorResponse |}];

  Printf.printf "%b" (Obj_spec.is_big x);
  [%expect {| false |}]


let%expect_test "Check enum" =
  let path = Q.path_of_json_pointer "/definitions/ErrorResponse/allOf/0" in
  let x = Enum_spec.of_path ~dirty_name:"ErrorResponse" ~path () in

  Printf.printf "%s" (Q.json_pointer_of_path x.path);
  [%expect {| /definitions/ErrorResponse/allOf/0 |}];

  let x = Enum_spec.set_enums x ~dirty_names:["type"; "module"; "lazy"; "null"; "multi Part"] in
  x.enums
  |> List.map (fun (nm:Enum_spec.enum_val) -> Printf.sprintf "%s:%s" nm.safe_name nm.dirty_name)
  |> String.concat ","
  |> Printf.printf "%s";
  [%expect {| Type_:type,Module_:module,Lazy_:lazy,Empty_:null,Multi_Part:multi Part |}];

  let x = Enum_spec.append_enum x ~dirty_name:"field" in
  x.enums
  |> List.map (fun (nm:Enum_spec.enum_val) -> Printf.sprintf "%s:%s" nm.safe_name nm.dirty_name)
  |> String.concat ","
  |> Printf.printf "%s";
  [%expect {| Field:field,Type_:type,Module_:module,Lazy_:lazy,Empty_:null,Multi_Part:multi Part |}]