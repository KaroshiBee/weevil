module type PROT   = module type of Tezos_protocol_010_PtGRANAD.Protocol
module type CTXT   = module type of Tezos_protocol_010_PtGRANAD.Protocol.Alpha_context
module type ENV    = module type of Tezos_protocol_010_PtGRANAD.Environment
module type ERR    = module type of Tezos_protocol_010_PtGRANAD.Environment.Error_monad
module type PLUGIN = module type of Tezos_protocol_plugin_010_PtGRANAD
module type CLIENT = module type of Tezos_client_010_PtGRANAD

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
    (Tezos_protocol_010_PtGRANAD.Protocol)
    (Tezos_protocol_010_PtGRANAD.Environment)
    (Tezos_client_010_PtGRANAD)
    (Tezos_protocol_plugin_010_PtGRANAD)

module type TEZ = module type of Tez

let protocol_str = "PtGRANADsDU8R9daYKAgWnQYAJ64omN1o3KMGVCykShA97vQbvV"
