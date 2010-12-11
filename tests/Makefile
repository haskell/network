package := network
version := $(shell awk '/^version:/{print $$2}' ../$(package).cabal)
ghc := ghc
lib := ../dist/build/libHS$(package)-$(version).a
package-conf := ../dist/package.conf.inplace

all: run-tests

run-tests: simple uri
	./simple
	./uri

simple: $(lib) Simple.hs
	ranlib $(lib)
	$(ghc) --make Simple.hs -threaded -o $@ -package-conf=$(package-conf)

uri: $(lib) uri001.hs
	ranlib $(lib)
	$(ghc) --make uri001.hs -threaded -o $@ -package-conf=$(package-conf)

clean:
	-find . \( -name '*.o' -o -name '*.hi' \) -exec rm {} \;
	-rm -f simple uri

.PHONY: all clean run-tests
