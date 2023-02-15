module type PROT   = module type of Tezos_protocol_014_PtKathma.Protocol
module type CTXT   = module type of Tezos_protocol_014_PtKathma.Protocol.Alpha_context
module type ENV    = module type of Tezos_protocol_014_PtKathma.Environment
module type ERR    = module type of Tezos_protocol_014_PtKathma.Environment.Error_monad
module type PLUGIN = module type of Tezos_protocol_plugin_014_PtKathma
module type CLIENT = module type of Tezos_client_014_PtKathma

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


module Tez014 = MakeTezos
    (Tezos_protocol_014_PtKathma.Protocol)
    (Tezos_protocol_014_PtKathma.Environment)
    (Tezos_client_014_PtKathma)
    (Tezos_protocol_plugin_014_PtKathma)

