(lang dune 3.0)
(formatting (enabled_for ocaml))
(wrapped_executables false)
(name weevil)

(version "0.1.3") ; TODO should same as Utilities.Defaults._VERSION

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
 (name weevil_mdb_016)
 (synopsis "Tezos: `weevil_mdb_016` binary - a tool for debugging Michelson code")
 (description "Supporting tool for the main Weevil debugger")
 (depends
   ocaml
   dune
   cmdliner
   tezos-base
   tezos-micheline
   tezos-protocol-016-PtMumbai
   tezos-client-016-PtMumbai
   tezos-protocol-plugin-016-PtMumbai
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
 (name weevil_mdb_012)
 (synopsis "Tezos: `weevil_mdb_012` binary - a tool for debugging Michelson code")
 (description "Supporting tool for the main Weevil debugger")
 (depends
   ocaml
   dune
   cmdliner
   tezos-base
   tezos-micheline
   tezos-protocol-012-Psithaca
   tezos-client-012-Psithaca
   tezos-protocol-plugin-012-Psithaca
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
 (name weevil_mdb_011)
 (synopsis "Tezos: `weevil_mdb_011` binary - a tool for debugging Michelson code")
 (description "Supporting tool for the main Weevil debugger")
 (depends
   ocaml
   dune
   cmdliner
   tezos-base
   tezos-micheline
   tezos-protocol-011-PtHangz2
   tezos-client-011-PtHangz2
   tezos-protocol-plugin-011-PtHangz2
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

;; NOTE we only go back as far as 011 because Tezos mockups are not registered in the codebase until 011.
;; history, mockups were announced in Dec 2020, so the earliest version that *could* have them would be
;; Florence - 009, however Florence and Granada - 010 do not register a mockup mode so they cannot be used
;; in the Weevil

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
