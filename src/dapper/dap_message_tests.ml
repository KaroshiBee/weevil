open Dap_message
open Dap_commands
module G = QCheck.Gen


let gen_writeMem = G.(
    tup4 gen_int31 (pure writeMemory) WriteMemoryArguments.gen (pure WriteMemoryArguments.enc)
  )

let gen_readMem = G.(
    tup4 gen_int31 (pure readMemory) ReadMemoryArguments.gen (pure ReadMemoryArguments.enc)
  )

let gen_stackTrace = G.(
    tup4 gen_int31 (pure stackTrace) StackTraceArguments.gen (pure StackTraceArguments.enc)
  )

let gen_requestMessage gen = G.(
    gen >|= fun (seq, command, arguments, enc) ->
    RequestMessage.make ~seq ~command ~arguments (), RequestMessage.enc command enc
  )
let gen_requestMessage_string gen = G.(
    gen_requestMessage gen >|= (fun (req, enc) -> Data_encoding.Json.(construct enc req |> to_string))
  )

let gen_request : type cmd args pargs. (cmd, args, pargs) request -> string QCheck.Gen.t
    = function
      | WriteMemoryRequest _ ->
        gen_requestMessage_string gen_writeMem
      | ReadMemoryRequest _ ->
        gen_requestMessage_string gen_readMem
      | StackTraceRequest _ ->
        gen_requestMessage_string gen_stackTrace
      | _ -> failwith "BOO"
