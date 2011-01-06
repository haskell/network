package := network
version := $(shell awk '/^version:/{print $$2}' ../$(package).cabal)
ghc := ghc
ghc-flags :=  -Wall -O -hide-all-packages \
	-package-conf ../dist/package.conf.inplace -package base \
	-package bytestring -package HUnit -package parsec -package $(package)
lib := ../dist/build/libHS$(package)-$(version).a
package-conf := ../dist/package.conf.inplace

programs := simple uri

all: run-tests

run-tests: simple uri
	./simple
	./uri

simple: $(lib) Simple.hs
	ranlib $(lib)
	$(ghc) $(ghc-flags) --make -threaded -o $@ Simple.hs

uri: $(lib) uri001.hs
	ranlib $(lib)
	$(ghc) $(ghc-flags) --make -threaded -o $@ uri001.hs

clean:
	-find . \( -name '*.o' -o -name '*.hi' \) -exec rm {} \;
	-rm -f $(programs)

.PHONY: all clean run-tests
