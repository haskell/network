/* Socket operations with an OVERLAPPED of the caller's, waited for here, so
   that CancelIoEx(s, ov) cancels this one alone.  Each answers like the
   call it wraps, leaving the error for WSAGetLastError. */

#include <string.h>
#include "HsNet.h"

int
hsnet_ovl_size(void)
{
    return sizeof(OVERLAPPED);
}

/* The low bit of hEvent keeps the completion off a completion port the
   socket may be associated with. */
static int
ovl_begin(LPOVERLAPPED ov, HANDLE *ev)
{
    memset(ov, 0, sizeof(*ov));
    *ev = CreateEventW(NULL, TRUE, FALSE, NULL);
    if (*ev == NULL) {
        WSASetLastError(GetLastError());
        return SOCKET_ERROR;
    }
    ov->hEvent = (HANDLE)((ULONG_PTR)*ev | 1);
    return 0;
}

/* r is what the call answered.  A pending operation is waited for, and
   then *n and *flags are what it completed with. */
static int
ovl_end(SOCKET s, LPOVERLAPPED ov, HANDLE ev, int r,
        DWORD *n, DWORD *flags, int *waited)
{
    int err = 0;
    *waited = 0;
    if (r == SOCKET_ERROR) {
        err = WSAGetLastError();
        if (err == WSA_IO_PENDING) {
            err = 0;
            *waited = 1;
            if (!WSAGetOverlappedResult(s, ov, n, TRUE, flags))
                err = WSAGetLastError();
        }
    }
    CloseHandle(ev);
    if (err != 0) {
        WSASetLastError(err);
        return SOCKET_ERROR;
    }
    return 0;
}

int
hsnet_ovl_recv(SOCKET s, char *buf, int len, LPOVERLAPPED ov)
{
    WSABUF b;
    DWORD n = 0, flags = 0;
    HANDLE ev;
    int r, waited;
    if (ovl_begin(ov, &ev) != 0) return SOCKET_ERROR;
    b.buf = buf;
    b.len = len;
    r = WSARecv(s, &b, 1, &n, &flags, ov, NULL);
    if (ovl_end(s, ov, ev, r, &n, &flags, &waited) != 0) return SOCKET_ERROR;
    return (int)n;
}

int
hsnet_ovl_recvfrom(SOCKET s, char *buf, int len,
                   struct sockaddr *from, int *fromlen, LPOVERLAPPED ov)
{
    WSABUF b;
    DWORD n = 0, flags = 0;
    HANDLE ev;
    int r, waited;
    if (ovl_begin(ov, &ev) != 0) return SOCKET_ERROR;
    b.buf = buf;
    b.len = len;
    r = WSARecvFrom(s, &b, 1, &n, &flags, from, fromlen, ov, NULL);
    if (ovl_end(s, ov, ev, r, &n, &flags, &waited) != 0) return SOCKET_ERROR;
    return (int)n;
}

int
hsnet_ovl_send(SOCKET s, char *buf, int len, LPOVERLAPPED ov)
{
    WSABUF b;
    DWORD n = 0, flags = 0;
    HANDLE ev;
    int r, waited;
    if (ovl_begin(ov, &ev) != 0) return SOCKET_ERROR;
    b.buf = buf;
    b.len = len;
    r = WSASend(s, &b, 1, &n, 0, ov, NULL);
    if (ovl_end(s, ov, ev, r, &n, &flags, &waited) != 0) return SOCKET_ERROR;
    return (int)n;
}

int
hsnet_ovl_sendv(SOCKET s, LPWSABUF bufs, DWORD count, LPOVERLAPPED ov)
{
    DWORD n = 0, flags = 0;
    HANDLE ev;
    int r, waited;
    if (ovl_begin(ov, &ev) != 0) return SOCKET_ERROR;
    r = WSASend(s, bufs, count, &n, 0, ov, NULL);
    if (ovl_end(s, ov, ev, r, &n, &flags, &waited) != 0) return SOCKET_ERROR;
    return (int)n;
}

int
hsnet_ovl_sendto(SOCKET s, char *buf, int len,
                 const struct sockaddr *to, int tolen, LPOVERLAPPED ov)
{
    WSABUF b;
    DWORD n = 0, flags = 0;
    HANDLE ev;
    int r, waited;
    if (ovl_begin(ov, &ev) != 0) return SOCKET_ERROR;
    b.buf = buf;
    b.len = len;
    r = WSASendTo(s, &b, 1, &n, 0, to, tolen, ov, NULL);
    if (ovl_end(s, ov, ev, r, &n, &flags, &waited) != 0) return SOCKET_ERROR;
    return (int)n;
}

/* Answers 0 and puts the bytes sent in *sent, as WSASendMsg does. */
int
hsnet_ovl_sendmsg(SOCKET s, LPWSAMSG msg, DWORD flags, LPDWORD sent,
                  LPOVERLAPPED ov)
{
    DWORD oflags = 0;
    HANDLE ev;
    int r, waited;
    *sent = 0;
    if (ovl_begin(ov, &ev) != 0) return SOCKET_ERROR;
    r = WSASendMsg(s, msg, flags, sent, ov, NULL);
    return ovl_end(s, ov, ev, r, sent, &oflags, &waited);
}

/* Answers 0 and puts the bytes received in *got, as WSARecvMsg does,
   and SOCKET_ERROR with WSAEMSGSIZE for a message cut short. */
int
hsnet_ovl_recvmsg(SOCKET s, LPWSAMSG msg, LPDWORD got, LPOVERLAPPED ov)
{
    DWORD flags = 0;
    HANDLE ev;
    int r, waited;
    *got = 0;
    if (ovl_begin(ov, &ev) != 0) return SOCKET_ERROR;
    r = WSARecvMsg(s, msg, got, ov, NULL);
    r = ovl_end(s, ov, ev, r, got, &flags, &waited);
    if (waited) {
        /* Completed later: the flags come from here, not from msg. */
        msg->dwFlags = flags;
        if (r == SOCKET_ERROR && WSAGetLastError() == WSAEMSGSIZE) {
            msg->dwFlags |= MSG_TRUNC;
            msg->Control.len = 0;
            msg->Control.buf = NULL;
        }
    }
    return r;
}
