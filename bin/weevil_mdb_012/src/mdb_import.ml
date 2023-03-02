module type PROT   = module type of Tezos_protocol_012_Psithaca.Protocol
module type CTXT   = module type of Tezos_protocol_012_Psithaca.Protocol.Alpha_context
module type ENV    = module type of Tezos_protocol_012_Psithaca.Environment
module type ERR    = module type of Tezos_protocol_012_Psithaca.Environment.Error_monad
module type PLUGIN = module type of Tezos_protocol_plugin_012_Psithaca
module type CLIENT = module type of Tezos_client_012_Psithaca

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
    (Tezos_protocol_012_Psithaca.Protocol)
    (Tezos_protocol_012_Psithaca.Environment)
    (Tezos_client_012_Psithaca)
    (Tezos_protocol_plugin_012_Psithaca)

module type TEZ = module type of Tez

let protocol_str = "Psithaca2MLRFYargivpo7YvUr7wUDqyxrdhC5CQq78mRvimz6A"
