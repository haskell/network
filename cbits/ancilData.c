/*
 *  Copyright(c), 2002 The GHC Team.
 */

#include "HsNet.h"

#if HAVE_STRUCT_MSGHDR_MSG_CONTROL || HAVE_STRUCT_MSGHDR_MSG_ACCRIGHTS /* until end */

/* 
 *  Support for transmitting file descriptors.
 *
 *  
 */
 

/*
 * sendmsg() and recvmsg() wrappers for transmitting
 * ancillary socket data.
 *
 * Doesn't provide the full generality of either, specifically:
 *
 *  - no support for scattered read/writes.
 *  - only possible to send one ancillary chunk of data at a time.
 *
 *
 * NOTE: recv/sendAncillary() is being phased out in preference
 *       of the more specific send/recvFd(), as the latter is
 *       really the only application of recv/sendAncillary() and
 *       stand the chance of being supported on platforms that
 *       don't have send/recvmsg() (but do have ioctl() support
 *       for this kind of thing, for instance.)
 *
 */

int
sendFd(int sock,
       int outfd)
{
  struct msghdr msg = {0};
  struct iovec iov[1];
  char  buf[2];
#if HAVE_STRUCT_MSGHDR_MSG_ACCRIGHTS
  msg.msg_accrights = (void*)&outfd;
  msg.msg_accrightslen = sizeof(int);
#else
  struct cmsghdr *cmsg;
  char ancBuffer[CMSG_SPACE(sizeof(int))];
  char* dPtr;
  
  msg.msg_control = ancBuffer;
  msg.msg_controllen = sizeof(ancBuffer);

  cmsg = CMSG_FIRSTHDR(&msg);
  cmsg->cmsg_level = SOL_SOCKET;
  cmsg->cmsg_type = SCM_RIGHTS;
  cmsg->cmsg_len = CMSG_LEN(sizeof(int));
  dPtr = (char*)CMSG_DATA(cmsg);
  
  *(int*)dPtr = outfd;
  msg.msg_controllen = cmsg->cmsg_len;
#endif

  buf[0] = 0; buf[1] = '\0';
  iov[0].iov_base = buf;
  iov[0].iov_len  = 2;

  msg.msg_iov = iov;
  msg.msg_iovlen = 1;
  
  return sendmsg(sock,&msg,0);
}

int
sendAncillary(int sock,
	      int level,
	      int type,
	      int flags,
	      void* data,
	      int len)
{
  struct msghdr msg = {0};
  struct iovec iov[1];
  char  buf[2];
#if HAVE_STRUCT_MSGHDR_MSG_ACCRIGHTS
  /* Contains the older BSD msghdr fields only, so no room
     for 'type' or 'level' data.
  */
  msg.msg_accrights = data;
  msg.msg_accrightslen=len;
#else
  struct cmsghdr *cmsg;
  char ancBuffer[CMSG_SPACE(len)];
  char* dPtr;
  
  msg.msg_control = ancBuffer;
  msg.msg_controllen = sizeof(ancBuffer);

  cmsg = CMSG_FIRSTHDR(&msg);
  cmsg->cmsg_level = level;
  cmsg->cmsg_type = type;
  cmsg->cmsg_len = CMSG_LEN(len);
  dPtr = (char*)CMSG_DATA(cmsg);
  
  memcpy(dPtr, data, len);
  msg.msg_controllen = cmsg->cmsg_len;
#endif
  buf[0] = 0; buf[1] = '\0';
  iov[0].iov_base = buf;
  iov[0].iov_len  = 2;

  msg.msg_iov = iov;
  msg.msg_iovlen = 1;
  
  return sendmsg(sock,&msg,flags);
}

int
recvFd(int sock)
{
  struct msghdr msg = {0};
  char  duffBuf[10];
  int rc;
  int len = sizeof(int);
  struct iovec iov[1];
#if HAVE_STRUCT_MSGHDR_MSG_CONTROL
  struct cmsghdr *cmsg = NULL;
  struct cmsghdr *cptr;
#else
  int* fdBuffer;
#endif

  iov[0].iov_base = duffBuf;
  iov[0].iov_len  = sizeof(duffBuf);
  msg.msg_iov = iov;
  msg.msg_iovlen = 1;

#if HAVE_STRUCT_MSGHDR_MSG_CONTROL
  cmsg = (struct cmsghdr*)malloc(CMSG_SPACE(len));
  if (cmsg==NULL) {
    return -1;
  }
  
  msg.msg_control = (void *)cmsg;
  msg.msg_controllen = CMSG_LEN(len);
#else
  fdBuffer = (int*)malloc(len);
  if (fdBuffer) {
    msg.msg_accrights    = (void *)fdBuffer;
  } else {
    return -1;
  }
  msg.msg_accrightslen = len;
#endif

  if ((rc = recvmsg(sock,&msg,0)) < 0) {
    return rc;
  }
  
#if HAVE_STRUCT_MSGHDR_MSG_CONTROL
  cptr = (struct cmsghdr*)CMSG_FIRSTHDR(&msg);
  return *(int*)CMSG_DATA(cptr);
#else
  return *(int*)fdBuffer;
#endif
}


int
recvAncillary(int  sock,
	      int* pLevel,
	      int* pType,
	      int  flags,
	      void** pData,
	      int* pLen)
{
  struct msghdr msg = {0};
  char  duffBuf[10];
  int rc;
  struct iovec iov[1];
#if HAVE_STRUCT_MSGHDR_MSG_CONTROL
  struct cmsghdr *cmsg = NULL;
  struct cmsghdr *cptr;
#endif
  
  iov[0].iov_base = duffBuf;
  iov[0].iov_len  = sizeof(duffBuf);
  msg.msg_iov = iov;
  msg.msg_iovlen = 1;

#if HAVE_STRUCT_MSGHDR_MSG_CONTROL
  cmsg = (struct cmsghdr*)malloc(CMSG_SPACE(*pLen));
  if (cmsg==NULL) {
    return -1;
  }
  
  msg.msg_control = (void *)cmsg;
  msg.msg_controllen = CMSG_LEN(*pLen);
#else
  *pData = (void*)malloc(*pLen);
  if (*pData) {
      msg.msg_accrights    = *pData;
  } else {
      return -1;
  }
  msg.msg_accrightslen = *pLen;
#endif

  if ((rc = recvmsg(sock,&msg,flags)) < 0) {
    return rc;
  }
  
#if HAVE_STRUCT_MSGHDR_MSG_CONTROL
  cptr = (struct cmsghdr*)CMSG_FIRSTHDR(&msg);

  *pLevel = cptr->cmsg_level;
  *pType  = cptr->cmsg_type;
  /* The length of the data portion only */
  *pLen   = cptr->cmsg_len - sizeof(struct cmsghdr);
  *pData  = CMSG_DATA(cptr);
#else
  /* Sensible defaults, I hope.. */
  *pLevel = 0;
  *pType  = 0;
#endif

  return rc;
}
#endif
