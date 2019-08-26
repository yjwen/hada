ROOT := $(dir $(lastword $(MAKEFILE_LIST)))


__dump:
	ghc -dverbose-core2core -c $(ROOT)examples/maybeplus.hs
dump:
	cd $(ROOT); cabal run hada -- -d examples/maybeplus.hs 
build:
	cd $(ROOT); cabal build
