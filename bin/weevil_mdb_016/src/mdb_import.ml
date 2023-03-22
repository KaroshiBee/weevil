module type PROT   = module type of Tezos_protocol_016_PtMumbai.Protocol
module type CTXT   = module type of Tezos_protocol_016_PtMumbai.Protocol.Alpha_context
module type ENV    = module type of Tezos_protocol_016_PtMumbai.Environment
module type ERR    = module type of Tezos_protocol_016_PtMumbai.Environment.Error_monad
module type PLUGIN = module type of Tezos_protocol_plugin_016_PtMumbai
module type CLIENT = module type of Tezos_client_016_PtMumbai

module MakeTezos (P:PROT) (Env:ENV) (C:CLIENT) (Plugin:PLUGIN) = struct
  module Prot = P
  module Env = Env
  module Ctxt = P.Alpha_context
  module Err = Env.Error_monad
  module Plugin = Plugin
  module Client = C
  module PP = C.Michelson_v1_printer
  module Parser = C.Michelson_v1_parser
end

module Tez = MakeTezos
    (Tezos_protocol_016_PtMumbai.Protocol)
    (Tezos_protocol_016_PtMumbai.Environment)
    (Tezos_client_016_PtMumbai)
    (Tezos_protocol_plugin_016_PtMumbai)

module type TEZ = module type of Tez

let protocol_str = "PtMumbai2TmsJHNGRkD8v8YDbtao7BLUC3wjASn1inAKLFCjaH1"
