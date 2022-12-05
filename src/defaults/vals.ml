let _DEFAULT_STEPPER_LOG_FILE = "weevil_stepper.log"
let _DEFAULT_MDB_LOG_FILE = "weevil_mdb.log"
let _DEFAULT_LISTEN_ADDRESS = "loopback"
let _DEFAULT_ADAPTER_PORT = 9000
let _DEFAULT_BACKEND_PORT = 9001
let _DEFAULT_BACKLOG = 10

let _DEFAULT_BACKEND_CMD =
  Printf.sprintf "dune exec -- weevil backend -v %d >%s 2>&1" _DEFAULT_BACKEND_PORT _DEFAULT_MDB_LOG_FILE

let _DEFAULT_STEPPER_CMD ~script_filename ~storage ~parameter
  = Printf.sprintf "dune exec -- weevil stepper -v --headless --storage \'%s\' --parameter \'%s\' %s"
    storage parameter script_filename

let _THE_THREAD_ID = 1
let _THE_FRAME_ID = 1
let _THE_FRAME_NAME = "main" (* TODO would probably be the method name or contract entry point name *)
let _THE_ONLY_SCOPE = ("Locals", 1) (* suggested -> | Arguments | Locals | Registers *)
let _THE_GAS_LOCAL = ("gas", 0)
let _THE_MICHELSON_STACK_LOCAL = ("stack", 1)
let _THE_VARIABLES_REFERENCE = 0
