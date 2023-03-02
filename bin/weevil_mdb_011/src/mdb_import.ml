module type PROT   = module type of Tezos_protocol_011_PtHangz2.Protocol
module type CTXT   = module type of Tezos_protocol_011_PtHangz2.Protocol.Alpha_context
module type ENV    = module type of Tezos_protocol_011_PtHangz2.Environment
module type ERR    = module type of Tezos_protocol_011_PtHangz2.Environment.Error_monad
module type PLUGIN = module type of Tezos_protocol_plugin_011_PtHangz2
module type CLIENT = module type of Tezos_client_011_PtHangz2

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
    (Tezos_protocol_011_PtHangz2.Protocol)
    (Tezos_protocol_011_PtHangz2.Environment)
    (Tezos_client_011_PtHangz2)
    (Tezos_protocol_plugin_011_PtHangz2)

module type TEZ = module type of Tez

let protocol_str = "PtHangz2aRngywmSRGGvrcTyMbbdpWdpFKuS4uMWxg2RaH9i1qx"
