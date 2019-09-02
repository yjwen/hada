ROOT := $(dir $(lastword $(MAKEFILE_LIST)))

# Directories following cabal's building convention.
OBJ-DIR := dist/build/hada/hada-tmp
BIN-DIR := dist/build/hada

HC := ghc
HC-OPTS = -package ghc -package parsec  -outputdir $(ROOT)$(OBJ-DIR) -i$(ROOT)$(OBJ-DIR) -i$(ROOT)src

BIN := $(ROOT)$(BIN-DIR)/hada

__dump:
	ghc -dverbose-core2core -c $(ROOT)examples/abs.hs
dump:
	cd $(ROOT); cabal run hada -- -d examples/maybeplus.hs 

build: $(BIN)

.PHONY: $(BIN)
$(BIN): $(ROOT)src/Main.hs
	$(HC) --make $(HC-OPTS) $^ -o $@
