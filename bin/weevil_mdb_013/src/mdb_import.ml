module type PROT   = module type of Tezos_protocol_013_PtJakart.Protocol
module type CTXT   = module type of Tezos_protocol_013_PtJakart.Protocol.Alpha_context
module type ENV    = module type of Tezos_protocol_013_PtJakart.Environment
module type ERR    = module type of Tezos_protocol_013_PtJakart.Environment.Error_monad
module type PLUGIN = module type of Tezos_protocol_plugin_013_PtJakart
module type CLIENT = module type of Tezos_client_013_PtJakart

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
    (Tezos_protocol_013_PtJakart.Protocol)
    (Tezos_protocol_013_PtJakart.Environment)
    (Tezos_client_013_PtJakart)
    (Tezos_protocol_plugin_013_PtJakart)

module type TEZ = module type of Tez

let protocol_str = "PtJakart2xVj7pYXJBXrqHgd82rdkLey5ZeeGwDgPp9rhQUbSqY"
