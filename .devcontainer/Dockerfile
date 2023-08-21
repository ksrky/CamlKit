FROM ocaml/opam:ubuntu-ocaml-4.14

# Install OCaml's build system "Dune"
RUN opam install dune
# Install parser generator "Menhir"
RUN opam install menhir
# Install LLVM. Make sure 
RUN sudo apt install clang-13 python3 cmake -y
RUN opam install llvm.13.0.0
# Install testing libraries
RUN opam install ppx_inline_test ppx_expect