#include "HsNet.h"

#if defined(HAVE_WINSOCK2_H) && !defined(__CYGWIN__)

/*
void c_network_select1Init(Select1Data *sd, SOCKET sock, int evtMask)
{
    sd->sock      = sock;
    sd->evtMask   = evtMask;
    sd->lastError = 0;
    sd->tv_ptr    = NULL;
}
*/

void c_network_select1InitTimeout(
    Select1Data *sd, SOCKET sock, int evtMask,
    long tv_sec, long tv_usec)
{
    sd->sock       = sock;
    sd->evtMask    = evtMask;
    sd->lastError  = 0;
    sd->tv_ptr     = &sd->tv;
    sd->tv.tv_sec  = tv_sec;
    sd->tv.tv_usec = tv_usec;
}

HsInt c_network_select1(Select1Data *sd)
{
    fd_set bufs[3], *sets[3];
    int i;
    SOCKET sock = sd->sock;
    int mask = sd->evtMask;
    int rc;

    for (i = 0; i < 3; i++) {
        if (mask & (1<<i)) {
            sets[i] = &bufs[i];
            FD_ZERO(sets[i]);
            FD_SET(sock, sets[i]);
        } else {
            sets[i] = NULL;
        }
    }

    rc = select( 0          /* nfds (ignored by Winsock) */
               , sets[0]    /* readfds */
               , sets[1]    /* writefds */
               , sets[2]    /* exceptfds */
               , sd->tv_ptr
               );

    /*
     * Update sd->evtMask to indicate what events occurred.
     * If no events occurred (error or timeout), set it to zero.
     */
    mask = 0;
    if (rc > 0) {
        for (i = 0; i < 3; i++) {
            if (sets[i] != NULL && FD_ISSET(sock, sets[i])) {
                mask |= (1<<i);
            }
        }
    } else if (rc < 0) {
        sd->lastError = WSAGetLastError();
    }
    sd->evtMask = mask;

    return rc;
}

#endif
