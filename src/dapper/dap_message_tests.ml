open Dap_message
open Dap_commands
module G = QCheck.Gen

let gen_cancel = G.(
    tup4 gen_int31 (pure cancel) CancelArguments.gen (pure CancelArguments.enc)
  )

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
let gen_requestMessage_opt gen = G.(
    gen >|= fun (seq, command, arguments, enc) ->
    RequestMessage.make_opt ~seq ~command ~arguments (), RequestMessage.enc_opt command enc
  )
let gen_requestMessage_string gen = G.(
    gen >|= (fun (req, enc) -> Data_encoding.Json.(construct enc req |> to_string))
  )

let gen_cancel_request = gen_requestMessage_string @@ gen_requestMessage_opt gen_cancel
let gen_write_memory_request = gen_requestMessage_string @@ gen_requestMessage gen_writeMem
let gen_read_memory_request = gen_requestMessage_string @@ gen_requestMessage gen_readMem
let gen_stack_trace_request = gen_requestMessage_string @@ gen_requestMessage gen_stackTrace

let gen_request = G.oneof [
    gen_cancel_request;
    gen_read_memory_request;
    gen_write_memory_request;
    gen_stack_trace_request;
  ]
