let _DEFAULT_LOG_FILE = "weevil_stepper.log"
let _DEFAULT_LISTEN_ADDRESS = "loopback"
let _DEFAULT_PORT = 9000

let _replace input output =
  Str.global_replace (Str.regexp_string input) output
