#include "HsNet.h"
#include <string.h>

#ifdef _WIN32

struct LPCMSGHDR cmsg_firsthdr(LPWSAMSG mhdr) {
  return (WSA_CMSG_FIRSTHDR(mhdr));
}

struct LPCMSGHDR cmsg_nxthdr(LPWSAMSG mhdr, LPWSACMSGHDR cmsg) {
  return (WSA_CMSG_NXTHDR(mhdr, cmsg));
}

unsigned char *cmsg_data(LPWSACMSGHDR cmsg) {
  return (WSA_CMSG_DATA(cmsg));
}

unsigned int cmsg_space(unsigned int l) {
  return (WSA_CMSG_SPACE(l));
}

unsigned int cmsg_len(unsigned int l) {
  return (WSA_CMSG_LEN(l));
}
#else
struct cmsghdr *cmsg_firsthdr(struct msghdr *mhdr) {
  return (CMSG_FIRSTHDR(mhdr));
}

struct cmsghdr *cmsg_nxthdr(struct msghdr *mhdr, struct cmsghdr *cmsg) {
  return (CMSG_NXTHDR(mhdr, cmsg));
}

unsigned char *cmsg_data(struct cmsghdr *cmsg) {
  return (CMSG_DATA(cmsg));
}

int cmsg_space(int l) {
  return (CMSG_SPACE(l));
}

int cmsg_len(int l) {
  return (CMSG_LEN(l));
}
#endif /* _WIN32 */
