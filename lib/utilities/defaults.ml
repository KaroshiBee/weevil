let _VERSION = "0.1.2" (* TODO should be the same as (version XXX) in dune-project *)

let _DEFAULT_STEPPER_LOG_FILE = "weevil_stepper.log"
let _DEFAULT_MDB_LOG_FILE = "weevil_mdb.log"
let _DEFAULT_LISTEN_ADDRESS = "loopback"
let _DEFAULT_ADAPTER_PORT = 9000
let _DEFAULT_BACKEND_PORT = 9001
let _DEFAULT_BACKLOG = 10

let _DEFAULT_BACKEND_CMD =
  Printf.sprintf "dune exec -- weevil backend -v -v %d >%s 2>&1" _DEFAULT_BACKEND_PORT _DEFAULT_MDB_LOG_FILE

let _DEFAULT_STEPPER_CMD ~script_filename ~storage ~parameter ~entrypoint
  = Printf.sprintf "dune exec -- weevil stepper -v -v --headless --storage \'%s\' --parameter \'%s\' --entrypoint \'%s\' %s"
    storage parameter entrypoint script_filename

(* theres only one (dummy) thread, it is the first thing requested for the waterfall *)
let _THE_THREAD_ID = 1

(* only one stack frame currently *)
let _THE_FRAME_ID = succ _THE_THREAD_ID
(* suggested -> | Arguments | Locals | Registers *)
let _THE_LOCALS_SCOPE = ("Script Locals", succ _THE_FRAME_ID)
let _THE_ARGS_SCOPE = ("Script Arguments", succ @@ snd _THE_LOCALS_SCOPE)

let _THE_PARAMETERS_LOCAL = ("parameter", succ @@ snd _THE_ARGS_SCOPE)
let _THE_STORAGE_LOCAL = ("storage", succ @@ snd _THE_PARAMETERS_LOCAL)
let _THE_GAS_LOCAL = ("gas", succ @@ snd _THE_STORAGE_LOCAL)
(* NOTE keep this as the last one, can add to it for the mich stack elements *)
let _THE_MICHELSON_STACK_LOCAL = ("stack", succ @@ snd _THE_GAS_LOCAL)

let classify_vref_exn vref =
  let locals = snd _THE_LOCALS_SCOPE in
  let args = snd _THE_ARGS_SCOPE in
  let params = snd _THE_PARAMETERS_LOCAL in
  let storage = snd _THE_STORAGE_LOCAL in
  let gas = snd _THE_GAS_LOCAL in
  let mich = snd _THE_MICHELSON_STACK_LOCAL in
  if vref = locals then `Locals
  else if vref = args then `Args
  else if vref = params then `Params
  else if vref = storage then `Storage
  else if vref = gas then `Gas
  else if vref = mich then `Mich_stack
  else
    raise @@ Invalid_argument (Printf.sprintf "Unknown vref %d" vref)
