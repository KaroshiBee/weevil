testing the backend with an example.tz michelson script, needs to step 8 times
multiplies 2 * 250, can run with $ cd ~/tezos && dune exec -- tezos-client --mode mockup run script ../dev/weevil/src/backend/tests/backend_cram_tests/stepper_test.t/multiply_2_x_250_equals_500.tz on storage 'Unit' and input 'Unit' --trace-stack
I send in 10 newlines to check that its ok to send them when it is finished

  $ echo "\n \n \n \n \n \n \n \n \n \n" | weevil_mdb_013 stepper --headless multiply_2_x_250_equals_500.tz
  Content-Length: 217
  
  { "location":    { "start": { "line": 3, "column": 9, "point": 43, "byte": 43 },      "stop": { "line": 3, "column": 13, "point": 47, "byte": 47 } },  "gas": "92.340 units remaining", "stack": [ "(Pair Unit Unit)" ] }Content-Length: 197
  
  { "location":    { "start": { "line": 3, "column": 9, "point": 43, "byte": 43 },      "stop": { "line": 3, "column": 13, "point": 47, "byte": 47 } },  "gas": "92.330 units remaining", "stack": [] }Content-Length: 204
  
  { "location":    { "start": { "line": 4, "column": 9, "point": 58, "byte": 58 },      "stop": { "line": 4, "column": 23, "point": 72, "byte": 72 } },  "gas": "92.320 units remaining", "stack": [ "250" ] }Content-Length: 209
  
  { "location":    { "start": { "line": 5, "column": 9, "point": 83, "byte": 83 },      "stop": { "line": 5, "column": 19, "point": 93, "byte": 93 } },  "gas": "92.310 units remaining", "stack": [ "2", "250" ] }Content-Length: 208
  
  { "location":    { "start": { "line": 6, "column": 9, "point": 104, "byte": 104 },      "stop": { "line": 6, "column": 12, "point": 107, "byte": 107 } },  "gas": "92.310 units remaining", "stack": [ "500" ] }Content-Length: 201
  
  { "location":    { "start": { "line": 7, "column": 9, "point": 118, "byte": 118 },      "stop": { "line": 7, "column": 13, "point": 122, "byte": 122 } },  "gas": "92.300 units remaining", "stack": [] }Content-Length: 209
  
  { "location":    { "start": { "line": 8, "column": 9, "point": 133, "byte": 133 },      "stop": { "line": 8, "column": 13, "point": 137, "byte": 137 } },  "gas": "92.290 units remaining", "stack": [ "Unit" ] }Content-Length: 215
  
  { "location":    { "start": { "line": 9, "column": 9, "point": 148, "byte": 148 },      "stop": { "line": 9, "column": 22, "point": 161, "byte": 161 } },  "gas": "92.280 units remaining", "stack": [ "{}", "Unit" ] }Content-Length: 221
  
  { "location":    { "start": { "line": 10, "column": 9, "point": 172, "byte": 172 },      "stop": { "line": 10, "column": 13, "point": 176, "byte": 176 } },  "gas": "92.270 units remaining", "stack": [ "(Pair {} Unit)" ] }

testing a contract that has a failwith, it should incrementally step up to it and then fail
  $ echo "\n \n \n \n \n \n \n \n \n \n" | weevil_mdb_013 stepper --headless --storage '0' --parameter '(Pair 7 5)' failwith.tz
  Content-Length: 220
  
  { "location":    { "start": { "line": 3, "column": 7, "point": 48, "byte": 48 },      "stop": { "line": 3, "column": 10, "point": 51, "byte": 51 } },  "gas": "91.309 units remaining", "stack": [ "(Pair (Pair 7 5) 0)" ] }Content-Length: 211
  
  { "location":    { "start": { "line": 3, "column": 7, "point": 48, "byte": 48 },      "stop": { "line": 3, "column": 10, "point": 51, "byte": 51 } },  "gas": "91.299 units remaining", "stack": [ "(Pair 7 5)" ] }Content-Length: 225
  
  { "location":    { "start": { "line": 4, "column": 7, "point": 61, "byte": 61 },      "stop": { "line": 4, "column": 10, "point": 64, "byte": 64 } },  "gas": "91.289 units remaining", "stack": [ "(Pair 7 5)", "(Pair 7 5)" ] }Content-Length: 221
  
  { "location":    { "start": { "line": 5, "column": 7, "point": 74, "byte": 74 },      "stop": { "line": 5, "column": 13, "point": 80, "byte": 80 } },  "gas": "91.279 units remaining", "stack": [ "7", "5", "(Pair 7 5)" ] }Content-Length: 216
  
  { "location":    { "start": { "line": 6, "column": 7, "point": 90, "byte": 90 },      "stop": { "line": 6, "column": 14, "point": 97, "byte": 97 } },  "gas": "91.244 units remaining", "stack": [ "1", "(Pair 7 5)" ] }Content-Length: 222
  
  { "location":    { "start": { "line": 7, "column": 7, "point": 107, "byte": 107 },      "stop": { "line": 7, "column": 9, "point": 109, "byte": 109 } },  "gas": "91.234 units remaining", "stack": [ "True", "(Pair 7 5)" ] }Content-Length: 215
  
  { "location":    { "start": { "line": 8, "column": 7, "point": 119, "byte": 119 },      "stop": { "line": 8, "column": 21, "point": 133, "byte": 133 } },  "gas": "91.224 units remaining", "stack": [ "(Pair 7 5)" ] }Content-Length: 212
  
  { "location":    { "start": { "line": 8, "column": 13, "point": 125, "byte": 125 },      "stop": { "line": 8, "column": 19, "point": 131, "byte": 131 } },  "gas": "91.214 units remaining", "stack": [ "7", "5" ] }Content-Length: 219
  
  { "location":    { "start": { "line": 8, "column": 7, "point": 119, "byte": 119 },      "stop": { "line": 8, "column": 21, "point": 133, "byte": 133 } },  "gas": "91.194 units remaining", "stack": [ "True", "7", "5" ] }Content-Length: 230
  
  { "location":    { "start": { "line": 9, "column": 7, "point": 143, "byte": 143 },      "stop": { "line": 9, "column": 24, "point": 160, "byte": 160 } },  "gas": "91.184 units remaining", "stack": [ "\"BOO\"", "True", "7", "5" ] }weevil_mdb_013: 
  Content-Length: 368
  
  { "error":    [ { "kind": "temporary",        "id": "proto.013-PtJakart.michelson_v1.runtime_error",        "contract_handle": "KT1BEqzn5Wx8uJrZNvuS9DVHmLvG9td3fDLi",        "contract_code": "Deprecated" },      { "kind": "temporary",        "id": "proto.013-PtJakart.michelson_v1.script_rejected",        "location": 20, "with": { "string": "BOO" }, "trace": [] } ] }
  [124]

testing with no filename and not headless
  $ weevil_mdb_013 stepper
  weevil_mdb_013: Stepper error - Error:
                                    Invalid_argument("required argument FILE is missing")
                  
  Usage: weevil_mdb_013 stepper [OPTION]… [FILE]
  Try 'weevil_mdb_013 stepper --help' or 'weevil_mdb_013 --help' for more information.
  [124]

testing with no filename in headless mode
  $ weevil_mdb_013 stepper --headless
  weevil_mdb_013: 
  Content-Length: 133
  
  { "error":    [ { "kind": "temporary", "id": "failure",        "msg": "Invalid_argument(\"required argument FILE is missing\")" } ] }
  [124]

testing with bad filename in headless mode
  $ weevil_mdb_013 stepper -h notthere.tz
  weevil_mdb_013: 
  Content-Length: 161
  
  { "error":    [ { "kind": "temporary", "id": "failure",        "msg":          "cannot read file (Unix.Unix_error(Unix.ENOENT, \"open\", \"notthere.tz\"))" } ] }
  [124]

testing with bad michelson in headless mode
  $ weevil_mdb_013 stepper -h bad_michelson.tz
  weevil_mdb_013: 
  Content-Length: 573
  
  { "error":    [ { "kind": "permanent", "id": "micheline.parse_error.unclosed_token",        "location":          { "start": { "line": 2, "column": 7, "point": 39, "byte": 39 },            "stop": { "line": 2, "column": 8, "point": 40, "byte": 40 } },        "token": { "punctuation": "{" } },      { "kind": "permanent", "id": "micheline.parse_error.unclosed_token",        "location":          { "start": { "line": 1, "column": 0, "point": 0, "byte": 0 },            "stop": { "line": 1, "column": 1, "point": 1, "byte": 1 } },        "token": { "punctuation": "{" } } ] }
  [124]


