testing with no filename and not headless
  $ for n in 015 014 013 012 011; do weevil_mdb_$n stepper; done
  weevil_mdb_015: Stepper error - Error:
                                    Invalid_argument("required argument FILE is missing")
                  
                  
  Usage: weevil_mdb_015 stepper [OPTION]… [FILE]
  Try 'weevil_mdb_015 stepper --help' or 'weevil_mdb_015 --help' for more information.
  weevil_mdb_014: Stepper error - Error:
                                    Invalid_argument("required argument FILE is missing")
                  
                  
  Usage: weevil_mdb_014 stepper [OPTION]… [FILE]
  Try 'weevil_mdb_014 stepper --help' or 'weevil_mdb_014 --help' for more information.
  weevil_mdb_013: Stepper error - Error:
                                    Invalid_argument("required argument FILE is missing")
                  
                  
  Usage: weevil_mdb_013 stepper [OPTION]… [FILE]
  Try 'weevil_mdb_013 stepper --help' or 'weevil_mdb_013 --help' for more information.
  weevil_mdb_012: Stepper error - Error:
                                    Invalid_argument("required argument FILE is missing")
                  
                  
  Usage: weevil_mdb_012 stepper [OPTION]… [FILE]
  Try 'weevil_mdb_012 stepper --help' or 'weevil_mdb_012 --help' for more information.
  weevil_mdb_011: Stepper error - Error:
                                    Invalid_argument("required argument FILE is missing")
                  
                  
  Usage: weevil_mdb_011 stepper [OPTION]… [FILE]
  Try 'weevil_mdb_011 stepper --help' or 'weevil_mdb_011 --help' for more information.
  [124]

testing with no filename in headless mode
  $ for n in 015 014 013 012 011; do weevil_mdb_$n stepper --headless; done
  Content-Length: 133
  
  { "error":    [ { "kind": "temporary", "id": "failure",        "msg": "Invalid_argument(\"required argument FILE is missing\")" } ] }
  weevil_mdb_015: 
  Content-Length: 133
  
  { "error":    [ { "kind": "temporary", "id": "failure",        "msg": "Invalid_argument(\"required argument FILE is missing\")" } ] }
  weevil_mdb_014: 
  Content-Length: 133
  
  { "error":    [ { "kind": "temporary", "id": "failure",        "msg": "Invalid_argument(\"required argument FILE is missing\")" } ] }
  weevil_mdb_013: 
  Content-Length: 133
  
  { "error":    [ { "kind": "temporary", "id": "failure",        "msg": "Invalid_argument(\"required argument FILE is missing\")" } ] }
  weevil_mdb_012: 
  Content-Length: 133
  
  { "error":    [ { "kind": "temporary", "id": "failure",        "msg": "Invalid_argument(\"required argument FILE is missing\")" } ] }
  weevil_mdb_011: 
  [124]

testing with bad filename in headless mode
  $ for n in 015 014 013 012 011; do weevil_mdb_$n stepper -h notthere.tz; done
  Content-Length: 161
  
  { "error":    [ { "kind": "temporary", "id": "failure",        "msg":          "cannot read file (Unix.Unix_error(Unix.ENOENT, \"open\", \"notthere.tz\"))" } ] }
  weevil_mdb_015: 
  Content-Length: 161
  
  { "error":    [ { "kind": "temporary", "id": "failure",        "msg":          "cannot read file (Unix.Unix_error(Unix.ENOENT, \"open\", \"notthere.tz\"))" } ] }
  weevil_mdb_014: 
  Content-Length: 161
  
  { "error":    [ { "kind": "temporary", "id": "failure",        "msg":          "cannot read file (Unix.Unix_error(Unix.ENOENT, \"open\", \"notthere.tz\"))" } ] }
  weevil_mdb_013: 
  Content-Length: 161
  
  { "error":    [ { "kind": "temporary", "id": "failure",        "msg":          "cannot read file (Unix.Unix_error(Unix.ENOENT, \"open\", \"notthere.tz\"))" } ] }
  weevil_mdb_012: 
  Content-Length: 161
  
  { "error":    [ { "kind": "temporary", "id": "failure",        "msg":          "cannot read file (Unix.Unix_error(Unix.ENOENT, \"open\", \"notthere.tz\"))" } ] }
  weevil_mdb_011: 
  [124]
