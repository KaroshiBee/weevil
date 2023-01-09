(* config to make expect_ppx work with Lwt  *)
module Lwt_io_run = struct
  type 'a t = 'a Lwt.t
end

module Lwt_io_flush = struct
  type 'a t = 'a Lwt.t
  let return x = Lwt.return x
  let bind x ~f = Lwt.bind x f
  let to_run x = x
end

module Expect_test_config :
  Expect_test_config_types.S
  with module IO_run = Lwt_io_run
   and module IO_flush = Lwt_io_flush
= struct
  module IO_run = Lwt_io_run
  module IO_flush = Lwt_io_flush
  let run x = Lwt_main.run (x ())
  let flushed () = Lwt_io.(buffered stdout = 0)
  let upon_unreleasable_issue = `CR
  let sanitize s = s
  (* NOTE opam publish seems to check against an older version of ppx_expect that is expecting this flush func *)
  let [@warning "-32"] flush () = Lwt.return_unit
end
