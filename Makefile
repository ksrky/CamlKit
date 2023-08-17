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

lib/runtime.ll:
	$(CLANG) -S -emit-llvm lib/runtime.c

.PHONY: compile
compile: dune-build lib/runtime.ll
	$(DUNE) exec camlkitopt $(f)
	$(CLANG) runtime.ll $(f:.mlkit=.ll) -o $(basename $(f))

.PHONY: clean
clean:
	rm runtime.ll