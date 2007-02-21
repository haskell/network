# This is a test script for use with GHC's testsuite framework, see
# http://darcs.haskell.org/testsuite

test('net001', only_compiler_types(['ghc']), compile_and_run, ['-package network'])
test('net002', compose(compose(only_compiler_types(['ghc']), extra_run_opts('3')), reqlib('network')),
	compile_and_run, ['-package network'])

test('uri001', compose(skip_if_fast, reqlib('HUnit')), compile_and_run, ['-package network -package HUnit'])
