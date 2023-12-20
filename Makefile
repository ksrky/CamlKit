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

.PHONY: compile-mono
compile:
	$(DUNE) exec camlkitopt $(f)
	$(CLANG) $(f:.mlkit=.ll) -o "main"

.PHONY: clean
clean:
	rm main