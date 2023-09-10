DUNE := dune
CLANG := clang

CAMLKIT := camlkit
CAMLKITOPT := camlkitopt

.PHONY: dune-build
dune-build:
	$(DUNE) build

.PHONY: dune-test
dune-test:
	$(DUNE) test

runtime.ll:
	$(CLANG) -S -emit-llvm lib/runtime.c

.PHONY: compile
compile: dune-build runtime.ll
	$(DUNE) exec camlkitopt $(f)
	$(CLANG) runtime.ll $(f:.mlkit=.ll) -o "main"

.PHONY: clean
clean:
	rm main runtime.ll