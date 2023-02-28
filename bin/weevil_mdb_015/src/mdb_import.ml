module type PROT   = module type of Tezos_protocol_015_PtLimaPt.Protocol
module type CTXT   = module type of Tezos_protocol_015_PtLimaPt.Protocol.Alpha_context
module type ENV    = module type of Tezos_protocol_015_PtLimaPt.Environment
module type ERR    = module type of Tezos_protocol_015_PtLimaPt.Environment.Error_monad
module type PLUGIN = module type of Tezos_protocol_plugin_015_PtLimaPt
module type CLIENT = module type of Tezos_client_015_PtLimaPt

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
    (Tezos_protocol_015_PtLimaPt.Protocol)
    (Tezos_protocol_015_PtLimaPt.Environment)
    (Tezos_client_015_PtLimaPt)
    (Tezos_protocol_plugin_015_PtLimaPt)

module type TEZ = module type of Tez

let protocol_str = "PtLimaPtLMwfNinJi9rCfDPWea8dFgTZ1MeJ9f1m2SRic6ayiwW"
