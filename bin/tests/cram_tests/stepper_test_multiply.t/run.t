testing the backend with an example.tz michelson script, needs to step 8 times
multiplies 2 * 250, can run with $ cd ~/tezos && dune exec -- tezos-client --mode mockup run script ../dev/weevil/src/backend/tests/backend_cram_tests/stepper_test.t/multiply_2_x_250_equals_500.tz on storage 'Unit' and input 'Unit' --trace-stack
I send in 10 newlines to check that its ok to send them when it is finished

  $ for n in 015 014 013 012; do echo "weevil_mdb_$n" && echo "\n \n \n \n \n \n \n \n \n \n" | weevil_mdb_$n stepper --headless multiply_2_x_250_equals_500.tz; done
  weevil_mdb_015
  Content-Length: 217
  
  { "location":    { "start": { "line": 3, "column": 9, "point": 43, "byte": 43 },      "stop": { "line": 3, "column": 13, "point": 47, "byte": 47 } },  "gas": "92.565 units remaining", "stack": [ "(Pair Unit Unit)" ] }
  Content-Length: 197
  
  { "location":    { "start": { "line": 3, "column": 9, "point": 43, "byte": 43 },      "stop": { "line": 3, "column": 13, "point": 47, "byte": 47 } },  "gas": "92.555 units remaining", "stack": [] }
  Content-Length: 204
  
  { "location":    { "start": { "line": 4, "column": 9, "point": 58, "byte": 58 },      "stop": { "line": 4, "column": 23, "point": 72, "byte": 72 } },  "gas": "92.545 units remaining", "stack": [ "250" ] }
  Content-Length: 209
  
  { "location":    { "start": { "line": 5, "column": 9, "point": 83, "byte": 83 },      "stop": { "line": 5, "column": 19, "point": 93, "byte": 93 } },  "gas": "92.535 units remaining", "stack": [ "2", "250" ] }
  Content-Length: 208
  
  { "location":    { "start": { "line": 6, "column": 9, "point": 104, "byte": 104 },      "stop": { "line": 6, "column": 12, "point": 107, "byte": 107 } },  "gas": "92.535 units remaining", "stack": [ "500" ] }
  Content-Length: 201
  
  { "location":    { "start": { "line": 7, "column": 9, "point": 118, "byte": 118 },      "stop": { "line": 7, "column": 13, "point": 122, "byte": 122 } },  "gas": "92.525 units remaining", "stack": [] }
  Content-Length: 209
  
  { "location":    { "start": { "line": 8, "column": 9, "point": 133, "byte": 133 },      "stop": { "line": 8, "column": 13, "point": 137, "byte": 137 } },  "gas": "92.515 units remaining", "stack": [ "Unit" ] }
  Content-Length: 215
  
  { "location":    { "start": { "line": 9, "column": 9, "point": 148, "byte": 148 },      "stop": { "line": 9, "column": 22, "point": 161, "byte": 161 } },  "gas": "92.505 units remaining", "stack": [ "{}", "Unit" ] }
  Content-Length: 221
  
  { "location":    { "start": { "line": 10, "column": 9, "point": 172, "byte": 172 },      "stop": { "line": 10, "column": 13, "point": 176, "byte": 176 } },  "gas": "92.495 units remaining", "stack": [ "(Pair {} Unit)" ] }
  weevil_mdb_014
  Content-Length: 217
  
  { "location":    { "start": { "line": 3, "column": 9, "point": 43, "byte": 43 },      "stop": { "line": 3, "column": 13, "point": 47, "byte": 47 } },  "gas": "92.565 units remaining", "stack": [ "(Pair Unit Unit)" ] }
  Content-Length: 197
  
  { "location":    { "start": { "line": 3, "column": 9, "point": 43, "byte": 43 },      "stop": { "line": 3, "column": 13, "point": 47, "byte": 47 } },  "gas": "92.555 units remaining", "stack": [] }
  Content-Length: 204
  
  { "location":    { "start": { "line": 4, "column": 9, "point": 58, "byte": 58 },      "stop": { "line": 4, "column": 23, "point": 72, "byte": 72 } },  "gas": "92.545 units remaining", "stack": [ "250" ] }
  Content-Length: 209
  
  { "location":    { "start": { "line": 5, "column": 9, "point": 83, "byte": 83 },      "stop": { "line": 5, "column": 19, "point": 93, "byte": 93 } },  "gas": "92.535 units remaining", "stack": [ "2", "250" ] }
  Content-Length: 208
  
  { "location":    { "start": { "line": 6, "column": 9, "point": 104, "byte": 104 },      "stop": { "line": 6, "column": 12, "point": 107, "byte": 107 } },  "gas": "92.535 units remaining", "stack": [ "500" ] }
  Content-Length: 201
  
  { "location":    { "start": { "line": 7, "column": 9, "point": 118, "byte": 118 },      "stop": { "line": 7, "column": 13, "point": 122, "byte": 122 } },  "gas": "92.525 units remaining", "stack": [] }
  Content-Length: 209
  
  { "location":    { "start": { "line": 8, "column": 9, "point": 133, "byte": 133 },      "stop": { "line": 8, "column": 13, "point": 137, "byte": 137 } },  "gas": "92.515 units remaining", "stack": [ "Unit" ] }
  Content-Length: 215
  
  { "location":    { "start": { "line": 9, "column": 9, "point": 148, "byte": 148 },      "stop": { "line": 9, "column": 22, "point": 161, "byte": 161 } },  "gas": "92.505 units remaining", "stack": [ "{}", "Unit" ] }
  Content-Length: 221
  
  { "location":    { "start": { "line": 10, "column": 9, "point": 172, "byte": 172 },      "stop": { "line": 10, "column": 13, "point": 176, "byte": 176 } },  "gas": "92.495 units remaining", "stack": [ "(Pair {} Unit)" ] }
  weevil_mdb_013
  Content-Length: 217
  
  { "location":    { "start": { "line": 3, "column": 9, "point": 43, "byte": 43 },      "stop": { "line": 3, "column": 13, "point": 47, "byte": 47 } },  "gas": "92.340 units remaining", "stack": [ "(Pair Unit Unit)" ] }
  Content-Length: 197
  
  { "location":    { "start": { "line": 3, "column": 9, "point": 43, "byte": 43 },      "stop": { "line": 3, "column": 13, "point": 47, "byte": 47 } },  "gas": "92.330 units remaining", "stack": [] }
  Content-Length: 204
  
  { "location":    { "start": { "line": 4, "column": 9, "point": 58, "byte": 58 },      "stop": { "line": 4, "column": 23, "point": 72, "byte": 72 } },  "gas": "92.320 units remaining", "stack": [ "250" ] }
  Content-Length: 209
  
  { "location":    { "start": { "line": 5, "column": 9, "point": 83, "byte": 83 },      "stop": { "line": 5, "column": 19, "point": 93, "byte": 93 } },  "gas": "92.310 units remaining", "stack": [ "2", "250" ] }
  Content-Length: 208
  
  { "location":    { "start": { "line": 6, "column": 9, "point": 104, "byte": 104 },      "stop": { "line": 6, "column": 12, "point": 107, "byte": 107 } },  "gas": "92.310 units remaining", "stack": [ "500" ] }
  Content-Length: 201
  
  { "location":    { "start": { "line": 7, "column": 9, "point": 118, "byte": 118 },      "stop": { "line": 7, "column": 13, "point": 122, "byte": 122 } },  "gas": "92.300 units remaining", "stack": [] }
  Content-Length: 209
  
  { "location":    { "start": { "line": 8, "column": 9, "point": 133, "byte": 133 },      "stop": { "line": 8, "column": 13, "point": 137, "byte": 137 } },  "gas": "92.290 units remaining", "stack": [ "Unit" ] }
  Content-Length: 215
  
  { "location":    { "start": { "line": 9, "column": 9, "point": 148, "byte": 148 },      "stop": { "line": 9, "column": 22, "point": 161, "byte": 161 } },  "gas": "92.280 units remaining", "stack": [ "{}", "Unit" ] }
  Content-Length: 221
  
  { "location":    { "start": { "line": 10, "column": 9, "point": 172, "byte": 172 },      "stop": { "line": 10, "column": 13, "point": 176, "byte": 176 } },  "gas": "92.270 units remaining", "stack": [ "(Pair {} Unit)" ] }
  weevil_mdb_012
  Content-Length: 217
  
  { "location":    { "start": { "line": 3, "column": 9, "point": 43, "byte": 43 },      "stop": { "line": 3, "column": 13, "point": 47, "byte": 47 } },  "gas": "91.735 units remaining", "stack": [ "(Pair Unit Unit)" ] }
  Content-Length: 197
  
  { "location":    { "start": { "line": 3, "column": 9, "point": 43, "byte": 43 },      "stop": { "line": 3, "column": 13, "point": 47, "byte": 47 } },  "gas": "91.725 units remaining", "stack": [] }
  Content-Length: 204
  
  { "location":    { "start": { "line": 4, "column": 9, "point": 58, "byte": 58 },      "stop": { "line": 4, "column": 23, "point": 72, "byte": 72 } },  "gas": "91.715 units remaining", "stack": [ "250" ] }
  Content-Length: 209
  
  { "location":    { "start": { "line": 5, "column": 9, "point": 83, "byte": 83 },      "stop": { "line": 5, "column": 19, "point": 93, "byte": 93 } },  "gas": "91.705 units remaining", "stack": [ "2", "250" ] }
  Content-Length: 208
  
  { "location":    { "start": { "line": 6, "column": 9, "point": 104, "byte": 104 },      "stop": { "line": 6, "column": 12, "point": 107, "byte": 107 } },  "gas": "91.705 units remaining", "stack": [ "500" ] }
  Content-Length: 201
  
  { "location":    { "start": { "line": 7, "column": 9, "point": 118, "byte": 118 },      "stop": { "line": 7, "column": 13, "point": 122, "byte": 122 } },  "gas": "91.695 units remaining", "stack": [] }
  Content-Length: 209
  
  { "location":    { "start": { "line": 8, "column": 9, "point": 133, "byte": 133 },      "stop": { "line": 8, "column": 13, "point": 137, "byte": 137 } },  "gas": "91.685 units remaining", "stack": [ "Unit" ] }
  Content-Length: 215
  
  { "location":    { "start": { "line": 9, "column": 9, "point": 148, "byte": 148 },      "stop": { "line": 9, "column": 22, "point": 161, "byte": 161 } },  "gas": "91.670 units remaining", "stack": [ "{}", "Unit" ] }
  Content-Length: 221
  
  { "location":    { "start": { "line": 10, "column": 9, "point": 172, "byte": 172 },      "stop": { "line": 10, "column": 13, "point": 176, "byte": 176 } },  "gas": "91.655 units remaining", "stack": [ "(Pair {} Unit)" ] }

