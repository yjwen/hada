run_parse: parse
	./parse
parse: parse.hs
	ghc -package ghc parse.hs
run_test: test
	./test
test: test.hs B.hs
	ghc test.hs -o test
