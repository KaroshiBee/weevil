testing the backend with an example.tz michelson script, needs to step 8 times
I send in 10 newlines to check that its ok to send them when it is finished

  $ echo "\n \n \n \n \n \n \n \n \n \n" | weevil stepper multiply_2_x_250_equals_500.tz
  
  # log_interp @ location 7
  
  # log_entry @ location 7
  # got ''
  # log_exit @ location 7, line 1
  { "location": 1, "gas": "92.555 units remaining", "stack": [ "" ] }
  
  # log_entry @ location 8
  # got ' '
  # log_exit @ location 8, line 2
  weevil: [INFO] 250
  { "location": 2, "gas": "92.545 units remaining", "stack": [ "250" ] }
  
  # log_entry @ location 11
  # got ' '
  # log_exit @ location 11, line 3
  weevil: [INFO] 2
  weevil: [INFO] 250
  { "location": 3, "gas": "92.535 units remaining", "stack": [ "2", " 250" ] }
  
  # log_entry @ location 14
  # got ' '
  # log_exit @ location 14, line 4
  weevil: [INFO] 500
  { "location": 4, "gas": "92.535 units remaining", "stack": [ "500" ] }
  
  # log_entry @ location 15
  # got ' '
  # log_exit @ location 15, line 5
  { "location": 5, "gas": "92.525 units remaining", "stack": [ "" ] }
  
  # log_entry @ location 16
  # got ' '
  # log_exit @ location 16, line 6
  weevil: [INFO] Unit
  { "location": 6, "gas": "92.515 units remaining", "stack": [ "Unit" ] }
  
  # log_entry @ location 17
  # got ' '
  # log_exit @ location 17, line 7
  weevil: [INFO] {}
  weevil: [INFO] Unit
  { "location": 7, "gas": "92.505 units remaining",  "stack": [ "{}", " Unit" ] }
  
  # log_entry @ location 19
  # got ' '
  # log_exit @ location 19, line 8
  weevil: [INFO] (Pair {} Unit)
  { "location": 8, "gas": "92.495 units remaining",  "stack": [ "(Pair {} Unit)" ] }
  
  # log_entry @ location 6
  # got ' '
  # log_control


testing with no filename
  $ weevil stepper
  tezos-weevil: Stepper error - Error:
                                  Invalid_argument("required argument FILE is missing")
                
  Usage: tezos-weevil stepper [--headless] [OPTION]… [FILE]
  Try 'tezos-weevil stepper --help' or 'tezos-weevil --help' for more information.
  [124]

testing with no filename in headless mode
  $ weevil stepper --headless
  tezos-weevil: { "error":
                    [ { "kind": "temporary", "id": "failure",
                        "msg": "Invalid_argument(\"required argument FILE is missing\")" } ] }
  [124]

testing with bad filename in headless mode
  $ weevil stepper -h notthere.tz
  weevil: [ERROR] Sys_error("notthere.tz: No such file or directory")
  tezos-weevil: { "error":
                    [ { "kind": "temporary", "id": "failure",
                        "msg": "Sys_error(\"notthere.tz: No such file or directory\")" } ] }
  [124]

testing with bad michelson in headless mode
  $ weevil stepper -h bad_michelson.tz
  tezos-weevil: { "error":
                    [ { "kind": "permanent", "id": "micheline.parse_error.unclosed_token",
                        "location":
                          { "start": { "line": 1, "column": 35, "point": 35, "byte": 35 },
                            "stop": { "line": 1, "column": 36, "point": 36, "byte": 36 } },
                        "token": { "punctuation": "{" } },
                      { "kind": "permanent", "id": "micheline.parse_error.unclosed_token",
                        "location":
                          { "start": { "line": 1, "column": 0, "point": 0, "byte": 0 },
                            "stop": { "line": 1, "column": 1, "point": 1, "byte": 1 } },
                        "token": { "punctuation": "{" } } ] }
  [124]


show the help
  $ weevil stepper --help
  TEZOS-WEEVIL-STEPPER(1)       Tezos-weevil Manual      TEZOS-WEEVIL-STEPPER(1)
  
  
  
  NNAAMMEE
         tezos-weevil-stepper - Run the Weevil Michelson stepper for the backend
         service
  
  SSYYNNOOPPSSIISS
         tteezzooss--wweeeevviill sstteeppppeerr [----hheeaaddlleessss] [_O_P_T_I_O_N]… [_F_I_L_E]
  
  DDEESSCCRRIIPPTTIIOONN
         Run the Weevil Michelson stepper for the backend service
  
  AARRGGUUMMEENNTTSS
         _F_I_L_E
             The Michelson contract filename that the weevil stepper will
             execute (required).
  
  OOPPTTIIOONNSS
         --hh, ----hheeaaddlleessss
             Run the tool in headless mode, this means that --help is not shown
             on error and any errors are returned as JSON
  
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
