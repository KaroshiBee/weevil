open Dap_specs


let%expect_test "Check obj" =
  let path = Q.path_of_json_pointer "/definitions/ErrorResponse/allOf/0" in
  let x = Obj_spec.of_path ~dirty_name:"ErrorResponse" ~path () in

  Printf.printf "%s" (Q.json_pointer_of_path x.path);
  [%expect {| /definitions/ErrorResponse/allOf/0 |}];

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
