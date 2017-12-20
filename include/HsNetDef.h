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

#if defined(WITH_WINSOCK)
# define CALLCONV stdcall
#else
# define CALLCONV ccall
#endif

#if defined(mingw32_HOST_OS)
# define SAFE_ON_WIN safe
#else
# define SAFE_ON_WIN unsafe
#endif

#endif /* HSNETDEF_H */
