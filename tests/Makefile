package := network
version := $(shell awk '/^version:/{print $$2}' ../$(package).cabal)
ghc := ghc
lib := ../dist/build/libHS$(package)-$(version).a
package-conf := ../dist/package.conf.inplace

all: run-tests

run-tests: tests
	./tests

tests: $(lib) Simple.hs
	ranlib $(lib)
	$(ghc) --make Simple -threaded -o $@ -package-conf=$(package-conf)

clean:
	-find . \( -name '*.o' -o -name '*.hi' \) -exec rm {} \;
	-rm -f tests

.PHONY: all clean run-tests
