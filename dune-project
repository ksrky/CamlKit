(lang dune 3.6)

(name CamlKit)

(generate_opam_files true)

(source
 (github ksrky/CamlKit))

(authors "ksrk")

(maintainers "ksrk")

(license LICENSE)

(documentation https://url/to/documentation)

(package
 (name CamlKit)
 (synopsis
  "CamlKit is the name of OCaml's subset language and its compiler.")
 (description "A longer description")
 (depends ocaml dune)
 (tags
  ("compiler" "programming language" "ocaml" "secd machine")))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/dune-files.html#dune-project

(using menhir 2.0)
