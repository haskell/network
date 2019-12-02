#include "HsNet.h"
#include <string.h>

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
