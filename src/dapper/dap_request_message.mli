open Dap_message_types

include REQUEST_T

module MakeRO : functor (REQ:REQUEST_T) ->
  REQUEST_READONLY_T with type ('cmd, 'args, 'presence) t = ('cmd, 'args, 'presence) REQ.t
