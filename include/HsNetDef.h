#ifndef HSNETDEF_H
#define HSNETDEF_H

#include "HsNetworkConfig.h"

/* ultra-evil... */
#undef PACKAGE_BUGREPORT
#undef PACKAGE_NAME
#undef PACKAGE_STRING
#undef PACKAGE_TARNAME
#undef PACKAGE_VERSION

#if defined(HAVE_WINSOCK2_H)
# define WITH_WINSOCK  1
#endif

#if !defined(mingw32_HOST_OS) && !defined(_WIN32)
# define DOMAIN_SOCKET_SUPPORT 1
#endif

/* stdcall is for Windows 32.
   Haskell FFI does not have a keyword for Windows 64.
   If ccall/stdcall is specified on Windows 64,
   GHC ignores it and use a proper ABI for Windows 64.
   But if stdcall is specified, GHC displays a warning.
   So, let's use ccall for Windows 64.
 */
#if defined(mingw32_HOST_OS)
# if defined(i386_HOST_ARCH)
#  define CALLCONV stdcall
# elif defined(x86_64_HOST_ARCH)
#  define CALLCONV ccall
# else
#  error Unknown mingw32 arch
# endif
#else
# define CALLCONV ccall
#endif
#if defined(mingw32_HOST_OS)
# define SAFE_ON_WIN safe
#else
# define SAFE_ON_WIN unsafe
#endif

#endif /* HSNETDEF_H */
