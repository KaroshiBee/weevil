testing the backend with an example.tz michelson script, needs to step 8 times
multiplies 2 * 250, can run with $ cd ~/tezos && dune exec -- tezos-client --mode mockup run script ../dev/weevil/src/backend/tests/backend_cram_tests/stepper_test.t/multiply_2_x_250_equals_500.tz on storage 'Unit' and input 'Unit' --trace-stack
I send in 10 newlines to check that its ok to send them when it is finished

  $ echo "\n \n \n \n \n \n \n \n \n \n" | weevil stepper --headless multiply_2_x_250_equals_500.tz
  Content-Length: 84
  
  { "location": 7, "gas": "92.565 units remaining",  "stack": [ "(Pair Unit Unit)" ] }
  Content-Length: 67
  
  { "location": 7, "gas": "92.555 units remaining", "stack": [ "" ] }
  Content-Length: 70
  
  { "location": 8, "gas": "92.545 units remaining", "stack": [ "250" ] }
  Content-Length: 77
  
  { "location": 11, "gas": "92.535 units remaining", "stack": [ "2", " 250" ] }
  Content-Length: 71
  
  { "location": 14, "gas": "92.535 units remaining", "stack": [ "500" ] }
  Content-Length: 68
  
  { "location": 15, "gas": "92.525 units remaining", "stack": [ "" ] }
  Content-Length: 72
  
  { "location": 16, "gas": "92.515 units remaining", "stack": [ "Unit" ] }
  Content-Length: 80
  
  { "location": 17, "gas": "92.505 units remaining",  "stack": [ "{}", " Unit" ] }
  Content-Length: 83
  
  { "location": 19, "gas": "92.495 units remaining",  "stack": [ "(Pair {} Unit)" ] }

testing a contract that has a failwith, it should incrementally step up to it and then fail
  $ echo "\n \n \n \n \n \n \n \n \n \n" | weevil stepper --headless --storage '0' --parameter '(Pair 7 5)' failwith.tz
  Content-Length: 87
  
  { "location": 9, "gas": "91.834 units remaining",  "stack": [ "(Pair (Pair 7 5) 0)" ] }
  Content-Length: 77
  
  { "location": 9, "gas": "91.824 units remaining", "stack": [ "(Pair 7 5)" ] }
  Content-Length: 94
  
  { "location": 10, "gas": "91.814 units remaining",  "stack": [ "(Pair 7 5)", " (Pair 7 5)" ] }
  Content-Length: 91
  
  { "location": 11, "gas": "91.804 units remaining",  "stack": [ "7", " 5", " (Pair 7 5)" ] }
  Content-Length: 85
  
  { "location": 12, "gas": "91.769 units remaining",  "stack": [ "1", " (Pair 7 5)" ] }
  Content-Length: 88
  
  { "location": 13, "gas": "91.759 units remaining",  "stack": [ "True", " (Pair 7 5)" ] }
  Content-Length: 79
  
  { "location": 14, "gas": "91.759 units remaining",  "stack": [ "(Pair 7 5)" ] }
  Content-Length: 75
  
  { "location": 16, "gas": "91.749 units remaining", "stack": [ "7", " 5" ] }
  Content-Length: 85
  
  { "location": 14, "gas": "91.729 units remaining",  "stack": [ "True", " 7", " 5" ] }
  Content-Length: 97
  
  { "location": 17, "gas": "91.719 units remaining",  "stack": [ "\"BOO\"", " True", " 7", " 5" ] }
  tezos-weevil: 
  Content-Length: 368
  
  { "error":    [ { "kind": "temporary",        "id": "proto.014-PtKathma.michelson_v1.runtime_error",        "contract_handle": "KT1BEqzn5Wx8uJrZNvuS9DVHmLvG9td3fDLi",        "contract_code": "Deprecated" },      { "kind": "temporary",        "id": "proto.014-PtKathma.michelson_v1.script_rejected",        "location": 20, "with": { "string": "BOO" }, "trace": [] } ] }
  [124]

testing with no filename and not headless
  $ weevil stepper
  tezos-weevil: Stepper error - Error:
                                  Invalid_argument("required argument FILE is missing")
                
  Usage: tezos-weevil stepper [OPTION]… [FILE]
  Try 'tezos-weevil stepper --help' or 'tezos-weevil --help' for more information.
  [124]

testing with no filename in headless mode
  $ weevil stepper --headless
  tezos-weevil: 
  Content-Length: 133
  
  { "error":    [ { "kind": "temporary", "id": "failure",        "msg": "Invalid_argument(\"required argument FILE is missing\")" } ] }
  [124]

testing with bad filename in headless mode
  $ weevil stepper -h notthere.tz
  tezos-weevil: 
  Content-Length: 161
  
  { "error":    [ { "kind": "temporary", "id": "failure",        "msg":          "cannot read file (Unix.Unix_error(Unix.ENOENT, \"open\", \"notthere.tz\"))" } ] }
  [124]

testing with bad michelson in headless mode
  $ weevil stepper -h bad_michelson.tz
  tezos-weevil: 
  Content-Length: 573
  
  { "error":    [ { "kind": "permanent", "id": "micheline.parse_error.unclosed_token",        "location":          { "start": { "line": 2, "column": 5, "point": 35, "byte": 35 },            "stop": { "line": 2, "column": 6, "point": 36, "byte": 36 } },        "token": { "punctuation": "{" } },      { "kind": "permanent", "id": "micheline.parse_error.unclosed_token",        "location":          { "start": { "line": 1, "column": 0, "point": 0, "byte": 0 },            "stop": { "line": 1, "column": 1, "point": 1, "byte": 1 } },        "token": { "punctuation": "{" } } ] }
  [124]


show the help
  $ weevil stepper --help
  TEZOS-WEEVIL-STEPPER(1)       Tezos-weevil Manual      TEZOS-WEEVIL-STEPPER(1)
  
  
  
  NNAAMMEE
         tezos-weevil-stepper - Run the Weevil Michelson stepper for the backend
         service
  
  SSYYNNOOPPSSIISS
         tteezzooss--wweeeevviill sstteeppppeerr [_O_P_T_I_O_N]… [_F_I_L_E]
  
  DDEESSCCRRIIPPTTIIOONN
         Run the Weevil Michelson stepper for the backend service
  
  AARRGGUUMMEENNTTSS
         _F_I_L_E
             The Michelson contract filename that the weevil stepper will
             incrementally execute (required).
  
  OOPPTTIIOONNSS
         ----ccoolloorr=_W_H_E_N (absent=aauuttoo)
             Colorize the output. _W_H_E_N must be one of aauuttoo, aallwwaayyss or nneevveerr.
  
         --hh, ----hheeaaddlleessss
             Run the tool in headless mode, this means that --help is not shown
             on error and any errors are returned as JSON
  
         --pp _M_I_C_H_E_L_S_O_N, ----ppaarraammeetteerr=_M_I_C_H_E_L_S_O_N
             The Michelson input parameter that the weevil stepper will execute
             with (default 'Unit').
  
         --qq, ----qquuiieett
             Be quiet. Takes over --vv and ----vveerrbboossiittyy.
  
         --ss _M_I_C_H_E_L_S_O_N, ----ssttoorraaggee=_M_I_C_H_E_L_S_O_N
             The initial Michelson storage that the weevil stepper will execute
             with (default 'Unit').
  
         --vv, ----vveerrbboossee
             Increase verbosity. Repeatable, but more than twice does not bring
             more.
  
         ----vveerrbboossiittyy=_L_E_V_E_L (absent=wwaarrnniinngg)
             Be more or less verbose. _L_E_V_E_L must be one of qquuiieett, eerrrroorr,
             wwaarrnniinngg, iinnffoo or ddeebbuugg. Takes over --vv.
  
  CCOOMMMMOONN OOPPTTIIOONNSS
         ----hheellpp[=_F_M_T] (default=aauuttoo)
             Show this help in format _F_M_T. The value _F_M_T must be one of aauuttoo,
             ppaaggeerr, ggrrooffff or ppllaaiinn. With aauuttoo, the format is ppaaggeerr or ppllaaiinn
             whenever the TTEERRMM env var is dduummbb or undefined.
  
         ----vveerrssiioonn
             Show version information.
  
  EEXXIITT SSTTAATTUUSS
         sstteeppppeerr exits with the following status:
  
         0   on success.
  
         123 on indiscriminate errors reported on standard error.
  
         124 on command line parsing errors.
  
         125 on unexpected internal errors (bugs).
  
  SSEEEE AALLSSOO
         tezos-weevil(1)
  
  
  
  Tezos-weevil 1.0                                       TEZOS-WEEVIL-STEPPER(1)
