FROM ocaml/opam:latest

# Install OCaml's build system "Dune"
RUN opam install dune
# Install parser generator "Menhir"
RUN opam install menhir
# Install LLVM
RUN sudo apt install clang-16 python3 cmake -y
RUN opam install llvm.16.0.6+nnp
# Install testing libraries
RUN opam install ppx_inline_test ppx_expect