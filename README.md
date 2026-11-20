![GitHub Actions status](https://github.com/haskell/network/workflows/Haskell%20CI/badge.svg)
# [`network`](http://hackage.haskell.org/package/network)

To build this package directly from git, you must run `autoreconf -i`.
And then use `cabal build` or `stack build`.

## Support Policy

### GHC

Since version 4.0, the `network` package supports GHC 9.4 or later.

### Windows

Since version 4.0, `network` needs GHC's WinIO I/O manager on Windows, with
a GHC whose WinIO handles overlapped I/O exceptions: 9.12.5 or later in 9.12,
or 9.14.2 or later.  The I/O manager is chosen by the executable, so link it
with

```
ghc-options: -threaded "-with-rtsopts=--io-manager=native"
```

or run it with `+RTS --io-manager=native -RTS`.  With No IO Manager (before
WinIO, `--io-manager=posix`), use `network` 3.2.  Sockets behind a non-IFS
Layered Service Provider, deprecated since Windows 8, are not supported.

We use MSYS to build this package on Windows.
To use the `network` package on Cygwin, use `stack`.

## Coding

### .hs files

If you need C macros created by "configure" or `CALLCONV`, put

```
#include "HsNetDef.h"
```

"HsNet.h" does not work well since Mac's cpp sucks.

### .hsc files

If you need `#peek`, `#poke` and others, create a `.hsc` file with

```
#include "HsNet.h"
```

`HsNet.h` includes `HsNetDef.h` and necessary C structures.
Unfortunately, `hsc2hs` does not convert C macros.
So, if you use `CALLCONV`, the following is also necessary:

```
##include "HsNetDef.h"
```
