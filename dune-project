(lang dune 3.0)
(formatting (enabled_for ocaml))
(wrapped_executables false)
(name weevil_mdb)

(version "0.1.2") ; TODO should same as Utilities.Defaults._VERSION

(generate_opam_files true)

(source
 (github KaroshiBee/weevil))

(authors "Simon Parry")

(maintainers "simon.parry@karoshibee.com")

(license MIT)

(documentation "https://github.com/KaroshiBee/weevil")

(package
 (name weevil_dapper)
 (synopsis "Tezos: `weevil dapper` binary - an implementation of the Debug Adapter Protocol in OCaml")
 (description "Supporting tooling for the Tezos weevil.")
 (allow_empty)
 (depends
   ocaml
   dune
   cmdliner
   conduit-lwt-unix
   lwt_ppx
   ppx_expect
   ppx_deriving_qcheck
   qcheck-alcotest
   bisect_ppx)
 (tags
  (tezos octez weevil ocaml)))

(package
 (name weevil_mdb)
 (synopsis "Tezos: `weevil_mdb` binary - a tool for debugging Michelson code")
 (description "Supporting tool for the main Weevil debugger")
 (allow_empty)
 (depends
   ocaml
   dune
   cmdliner
   tezos-base
   tezos-client-base-unix
   tezos-micheline
   tezos-stdlib-unix
   tezos-rpc-http-client-unix
   conduit-lwt-unix
   weevil_dapper
   lwt_ppx
   ppx_expect
   ppx_deriving_qcheck
   qcheck-alcotest
   bisect_ppx)
 (tags
  (tezos octez weevil ocaml)))

(package
 (name weevil_mdb_015)
 (synopsis "Tezos: `weevil_mdb_015` binary - a tool for debugging Michelson code")
 (description "Supporting tool for the main Weevil debugger")
 (depends
   ocaml
   dune
   cmdliner
   tezos-base
   tezos-micheline
   tezos-protocol-015-PtLimaPt
   tezos-client-015-PtLimaPt
   tezos-protocol-plugin-015-PtLimaPt
   conduit-lwt-unix
   weevil_dapper
   weevil_mdb
   lwt_ppx
   ppx_expect
   ppx_deriving_qcheck
   qcheck-alcotest
   bisect_ppx)
 (tags
  (tezos octez weevil ocaml)))

(package
 (name weevil_mdb_014)
 (synopsis "Tezos: `weevil_mdb_014` binary - a tool for debugging Michelson code")
 (description "Supporting tool for the main Weevil debugger")
 (depends
   ocaml
   dune
   cmdliner
   tezos-base
   tezos-micheline
   tezos-protocol-014-PtKathma
   tezos-client-014-PtKathma
   tezos-protocol-plugin-014-PtKathma
   conduit-lwt-unix
   weevil_dapper
   weevil_mdb
   lwt_ppx
   ppx_expect
   ppx_deriving_qcheck
   qcheck-alcotest
   bisect_ppx)
 (tags
  (tezos octez weevil ocaml)))

(package
 (name weevil_mdb_013)
 (synopsis "Tezos: `weevil_mdb_013` binary - a tool for debugging Michelson code")
 (description "Supporting tool for the main Weevil debugger")
 (depends
   ocaml
   dune
   cmdliner
   tezos-base
   tezos-micheline
   tezos-protocol-013-PtJakart
   tezos-client-013-PtJakart
   tezos-protocol-plugin-013-PtJakart
   conduit-lwt-unix
   weevil_dapper
   weevil_mdb
   lwt_ppx
   ppx_expect
   ppx_deriving_qcheck
   qcheck-alcotest
   bisect_ppx)
 (tags
  (tezos octez weevil ocaml)))

(package
 (name weevil)
 (synopsis "Tezos: `weevil` binary - a tool for debugging Michelson code")
 (description "Tooling for debugging Michelson smart contracts")
 (depends
   ocaml
   dune
   cmdliner
   conduit-lwt-unix
   weevil_dapper
   weevil_mdb
   lwt_ppx
   ppx_expect
   ppx_deriving_qcheck
   qcheck-alcotest
   bisect_ppx)
 (tags
  (tezos octez weevil ocaml)))


; See the complete stanza docs at https://dune.readthedocs.io/en/stable/dune-files.html#dune-project
