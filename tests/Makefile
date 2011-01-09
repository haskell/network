package := network
version := $(shell awk '/^version:/{print $$2}' ../$(package).cabal)
ghc := ghc
ghc-flags :=  -Wall -O -fno-warn-unused-do-bind -hide-all-packages \
	-package-conf ../dist/package.conf.inplace -package base \
	-package bytestring -package HUnit -package parsec -package $(package) \
	-fno-warn-missing-signatures -package test-framework \
	-package test-framework-hunit
lib := ../dist/build/libHS$(package)-$(version).a
package-conf := ../dist/package.conf.inplace

programs := simple uri

all: run-tests

run-tests: $(programs)
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
