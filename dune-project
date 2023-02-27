(lang dune 3.0)
(formatting (enabled_for ocaml))
(wrapped_executables false)
(name weevil)

(version "0.1.2")

(generate_opam_files true)

(source
 (github KaroshiBee/weevil))

(authors "Simon Parry")

(maintainers "simon.parry@karoshibee.com")

(license MIT)

(documentation "https://github.com/KaroshiBee/weevil")

(package
 (name weevil)
 (synopsis "Tezos: `weevil` binary - a tool for debugging Michelson code")
 (description "Tooling for debugging Michelson smart contracts")
 (depends
   ocaml
   dune
   cmdliner
   tezos-base
   tezos-client-base-unix
   tezos-micheline
   tezos-stdlib-unix
   tezos-rpc-http-client-unix
   tezos-protocol-014-PtKathma
   tezos-client-014-PtKathma
   tezos-protocol-plugin-014-PtKathma
   tezos-protocol-013-PtJakart
   tezos-client-013-PtJakart
   tezos-protocol-plugin-013-PtJakart
   conduit-lwt-unix
   weevil_dapper
   weevil_mdb_014
   lwt_ppx
   ppx_expect
   ppx_deriving_qcheck
   qcheck-alcotest
   bisect_ppx)
 (tags
  (tezos octez weevil ocaml)))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/dune-files.html#dune-project
