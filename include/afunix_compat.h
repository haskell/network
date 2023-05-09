/* The version of afunix.h provided by the version of MSYS2 included with x86
 * versions of GHC before GHC 9.2 excludes certain components introduced with
 * Windows Vista.
 */

#ifndef AFUNIX_COMPAT_H
#define AFUNIX_COMPAT_H

#if defined(_AFUNIX_) || !defined(_WIN32) || __GLASGOW_HASKELL__ > 902
# include <afunix.h>
#else

#define UNIX_PATH_MAX 108

typedef struct sockaddr_un {
  ADDRESS_FAMILY sun_family;
  char sun_path[UNIX_PATH_MAX];
} SOCKADDR_UN, *PSOCKADDR_UN;

#define SIO_AF_UNIX_GETPEERPID _WSAIOR(IOC_VENDOR, 256)

#endif /* GHC version check */
#endif /* AFUNIX_COMPAT_H */