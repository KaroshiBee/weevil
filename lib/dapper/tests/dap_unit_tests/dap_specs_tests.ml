open Dap_specs
open Alcotest

module MakeModuleNameTests = struct
  let path = Q.path_of_json_pointer "/definitions/ErrorResponse/allOf/0/anyOf/1/oneOf/2/-/not/properties/thing"

  let test_module_name () =
    let actual = make_module_name path in
    check string "correct module name" "ErrorResponse_thing" actual

end

module ObjectSpecTests = struct
  let path = Q.path_of_json_pointer "/definitions/ErrorResponse/allOf/0"
  let fields = [
    Field_spec.make ~path:(Q.path_of_json_pointer "/definitions/capabilities") ~dirty_name:"type" ~required:true ~kind:`Other () ;
  ]
  let x = Obj_spec.of_path ~dirty_name:"ErrorResponse" ~path ~fields ~is_cyclic:true ()

  let test_pprint () =
    let actual = Printf.sprintf "%s" @@ Obj_spec.show x in
    let expected = "{ Dap_specs.Obj_spec.safe_name = \"ErrorResponse\";\n  dirty_name = \"ErrorResponse\"; path = /definitions/ErrorResponse/allOf/0;\n  fields =\n  [{ Dap_specs.Field_spec.safe_name = \"type_\"; dirty_name = \"type\";\n     path = /definitions/capabilities; module_name = \"\"; type_ = \"\";\n     enc_ = \"\"; required = true; cyclic = false; seq = false; kind = `Other }\n    ];\n  is_cyclic = true }" in
    check string "pprint obj spec" expected actual

  let test_path () =
    let actual = Q.json_pointer_of_path x.path in
    check string "correct path" "/definitions/ErrorResponse/allOf/0" actual

  let test_is_big () =
    let actual = Obj_spec.is_big x in
    check bool "is big" false actual
end

module EnumSpecTests = struct
  let path = Q.path_of_json_pointer "/definitions/ErrorResponse/allOf/0"
  let x = Enum_spec.of_path ~dirty_name:"ErrorResponse" ~path ~dirty_names:["thing"; "thang"; "thong"] ()

  let test_pprint () =
    let actual = Printf.sprintf "%s" @@ Enum_spec.show x in
    let expected = "{ Dap_specs.Enum_spec.safe_name = \"ErrorResponse\";\n  dirty_name = \"ErrorResponse\"; path = /definitions/ErrorResponse/allOf/0;\n  enums =\n  [{ Dap_specs.Enum_spec.safe_name = \"Thing\"; dirty_name = \"thing\" };\n    { Dap_specs.Enum_spec.safe_name = \"Thang\"; dirty_name = \"thang\" };\n    { Dap_specs.Enum_spec.safe_name = \"Thong\"; dirty_name = \"thong\" }];\n  suggested = false }" in
    check string "pprint enum spec" expected actual

  let test_path () =
    let actual = Q.json_pointer_of_path x.path in
    check string "correct path" "/definitions/ErrorResponse/allOf/0" actual

  let test_set_names () =
    let y = Enum_spec.set_enums x ~dirty_names:["type"; "module"; "lazy"; "variable"; "null"; "multi Part"] in
    let actual =
      y.enums
      |> List.map (fun (nm:Enum_spec.enum_val) -> Printf.sprintf "%s:%s" nm.safe_name nm.dirty_name)
      |> String.concat ","
    in
    let expected = "Type_:type,Module_:module,Lazy_:lazy,Variable_:variable,Empty_:null,Multi_Part:multi Part" in
    check string "correct enum names" expected actual

  let test_append_name () =
    let z = Enum_spec.append_enum x ~dirty_name:"field" in
    let actual =
      z.enums
      |> List.map (fun (nm:Enum_spec.enum_val) -> Printf.sprintf "%s:%s" nm.safe_name nm.dirty_name)
      |> String.concat ","
    in
    let expected = "Field:field,Thing:thing,Thang:thang,Thong:thong" in
    check string "correct enum names" expected actual

  let test_append_names () =
    let z = Enum_spec.append_enums x ~enums:[{safe_name="example"; dirty_name=" example "}] in
    let actual =
      z.enums
      |> List.map (fun (nm:Enum_spec.enum_val) -> Printf.sprintf "%s:%s" nm.safe_name nm.dirty_name)
      |> String.concat ","
    in
    let expected = "_example_: example ,Thing:thing,Thang:thang,Thong:thong" in
    check string "correct enum names" expected actual

end


module SpecsTests = struct
  let path = Q.path_of_json_pointer "/definitions/ErrorResponse/allOf/0/oneOf/1/anyOf/2/not/3/properties"
  let x_enum = make ~dirty_name:"boo" ~path ~dirty_names:["bla"; "blu"; "bli"] ()
  let x_obj = make ~dirty_name:"baa" ~path ()

  let test_to_string () =
    let actual = to_string x_enum in
    let expected = "(Dap_specs.Enum\n   { Dap_specs.Enum_spec.safe_name = \"boo\"; dirty_name = \"boo\";\n     path =\n     /definitions/ErrorResponse/allOf/0/oneOf/1/anyOf/2/not/3/properties;\n     enums =\n     [{ Dap_specs.Enum_spec.safe_name = \"Bla\"; dirty_name = \"bla\" };\n       { Dap_specs.Enum_spec.safe_name = \"Blu\"; dirty_name = \"blu\" };\n       { Dap_specs.Enum_spec.safe_name = \"Bli\"; dirty_name = \"bli\" }];\n     suggested = false })" in
    check string "correct to string" expected actual

  let test_dirty_name () =
    match x_enum with
    | Enum spec ->
      let actual = dirty_name ~path:spec.path |> Option.get in
      check string "correct dirty name" "ErrorResponse" actual
    | _ -> assert false

  let test_is_special () =
    match x_obj with
    | Object spec ->
      let actual = is_special_definition ~path:spec.path in
      check bool "correct dirty name" false actual
    | _ -> assert false
end
