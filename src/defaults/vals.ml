let _DEFAULT_LOG_FILE = "weevil_stepper.log"
let _DEFAULT_LISTEN_ADDRESS = "loopback"
let _DEFAULT_ADAPTER_PORT = 9000
let _DEFAULT_BACKEND_PORT = 9001
let _DEFAULT_BACKLOG = 10

let _DEFAULT_BACKEND_CMD = "dune exec -- ./src/main.exe backend"
let _DEFAULT_STEPPER_CMD = "dune exec -- ./src/main.exe stepper example.tz"
let _DEFAULT_BACKEND_ECHO = "echo 1"

let _THE_THREAD_ID = 1
let _THE_FRAME_ID = 1
let _THE_FRAME_NAME = "main" (* TODO would probably be the method name or contract entry point name *)
let _THE_ONLY_SCOPE = ("Locals", 1) (* suggested -> | Arguments | Locals | Registers *)
let _THE_GAS_LOCAL = ("gas", 0)
let _THE_MICHELSON_STACK_LOCAL = ("stack", 1)

