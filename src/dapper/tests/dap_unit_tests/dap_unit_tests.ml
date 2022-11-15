open Alcotest

let () =
  run "Dap_specs" [
    "path wrangling", [
      test_case "module names" `Quick Dap_specs_tests.MakeModuleNameTests.test_module_name;
      test_case "object path" `Quick Dap_specs_tests.ObjectSpecTests.test_path;
      test_case "enum path" `Quick Dap_specs_tests.EnumSpecTests.test_path;
      test_case "enum set path names" `Quick Dap_specs_tests.EnumSpecTests.test_set_names;
      test_case "enum append path names" `Quick Dap_specs_tests.EnumSpecTests.test_append_name;
    ];

    "object properties", [
      test_case "object is big" `Quick Dap_specs_tests.ObjectSpecTests.test_is_big;
      test_case "show object" `Quick Dap_specs_tests.ObjectSpecTests.test_pprint;
    ]
  ]
