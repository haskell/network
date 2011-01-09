Low-level networking interface
==============================

Building
--------

To build this package using Cabal directly from git, you must run
"autoreconf" before the usual Cabal build steps (configure/build/install).
autoreconf is included in the GNU autoconf tools.  There is no need to run
the "configure" script: the "setup configure" step will do this for you.

Running the test suite
----------------------

To run the test suite simply run:

    cabal configure --enable-tests
    cabal build
    cabal test
