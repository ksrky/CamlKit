FROM ocaml/opam

# Install OCaml's build system "Dune"
RUN opam install dune
# Install parser generator "Menhir"
RUN opam install menhir
# Install testing libraries
RUN opam install ppx_inline_test ppx_expect