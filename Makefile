ROOT := $(dir $(lastword $(MAKEFILE_LIST)))


__dump:
	ghc -dverbose-core2core -c $(ROOT)examples/maybeplus.hs

