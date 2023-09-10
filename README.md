# CamlKit

Compiler for a subst of OCaml for implementation studies. The compiler has two backend: an SECD machine backend with a bytecode interpreter and an LLVM backend which optimizes and generates LLVM IR.

You can build and install excutables by the following command:

```
dune install CamlKit
```

## Syntax

$$
\begin{align}
&prog ::=& &exp \nonumber \\
\nonumber \\
&exp ::=& &id \nonumber \\
&&        &nil \nonumber \\
&&        &true\ |\ false \nonumber \\
&&        &integer \nonumber \\
&&        &exp\ exp \nonumber \\
&&        &\texttt{fun}\ id\ ...\ id\ \texttt{->}\ exp \nonumber \\
&&        &exp\ op\ exp \nonumber \\
&&        &\texttt{if}\ exp\ \texttt{then}\ exp\ \texttt{else}\ exp \nonumber \\
&&        &\texttt{let}\ bnds\ \texttt{in}\ exp \nonumber \\
&&        &\texttt{let}\ \texttt{rec}\ bnds\ \texttt{in}\ exp \nonumber \\
\nonumber \\
&bnds ::=& &bnd\ \texttt{and}\ bnds \nonumber \\
&&         &bnd \nonumber \\

&bnd ::=& &id\ \texttt{=}\ exp \nonumber
\end{align}
$$

## SECD Backend

### Compilation flow

[Source] -> Parse -> [AST] -> Type checking -> [IR] -> Compilation to SECD -> [SECD instructions]

### Execution

Compile and run on SECD virual machine.

```
$ camlkit ex/ex4.mlkit
120
```

or use interactive mode.

```
$ camlkit
#
```

## LLVM backend

### Compilation flow

[Source] -> Parse -> [AST] -> Type checking -> [IR] -> Optimization -> Closure conversion & Lambda lifting -> Code generation -> [LLVM IR]

### List of optimizations

- Inlining of small or infrequently used functions
- Dead variable elimination
- Uncurrying and expansion of nested let constructs
- Additional optimizations provided by LLVM, such as constant folding.

### Execution

```
$ make compile f=ex/ex7.mlkit
dune build
clang -S -emit-llvm lib/runtime.c
dune exec camlkitopt ex/ex7.mlkit
clang runtime.ll ex/ex7.ll -o "main"
$ ./main
$ make clean
rm main runtime.ll
```
