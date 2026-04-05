#include "HsNet.h"

LPFN_CONNECTEX loadConnectEx(SOCKET sock)
{
    GUID guid = WSAID_CONNECTEX;
    LPFN_CONNECTEX cex = NULL;
    DWORD bytes;

    WSAIoctl(sock, SIO_GET_EXTENSION_FUNCTION_POINTER,
            &guid, sizeof(guid),
            &cex, sizeof(cex),
            &bytes, NULL, NULL);
    return cex;
}

LPFN_ACCEPTEX loadAcceptEx(SOCKET sock)
{
    GUID guid = WSAID_ACCEPTEX;
    LPFN_ACCEPTEX fn = NULL;
    DWORD bytes;

    WSAIoctl(sock, SIO_GET_EXTENSION_FUNCTION_POINTER,
            &guid, sizeof(guid),
            &fn, sizeof(fn),
            &bytes, NULL, NULL);
    return fn;
}

LPFN_GETACCEPTEXSOCKADDRS loadGetAcceptExSockaddrs(SOCKET sock)
{
    GUID guid = WSAID_GETACCEPTEXSOCKADDRS;
    LPFN_GETACCEPTEXSOCKADDRS fn = NULL;
    DWORD bytes;

    WSAIoctl(sock, SIO_GET_EXTENSION_FUNCTION_POINTER,
            &guid, sizeof(guid),
            &fn, sizeof(fn),
            &bytes, NULL, NULL);
    return fn;
}

LPFN_WSASENDMSG loadWSASendMsg(SOCKET sock)
{
    GUID guid = WSAID_WSASENDMSG;
    LPFN_WSASENDMSG fn = NULL;
    DWORD bytes;

    WSAIoctl(sock, SIO_GET_EXTENSION_FUNCTION_POINTER,
            &guid, sizeof(guid),
            &fn, sizeof(fn),
            &bytes, NULL, NULL);
    return fn;
}

LPFN_WSARECVMSG loadWSARecvMsg(SOCKET sock)
{
    GUID guid = WSAID_WSARECVMSG;
    LPFN_WSARECVMSG fn = NULL;
    DWORD bytes;

    WSAIoctl(sock, SIO_GET_EXTENSION_FUNCTION_POINTER,
            &guid, sizeof(guid),
            &fn, sizeof(fn),
            &bytes, NULL, NULL);
    return fn;
}

/// Create an unbound socket matching the given listen socket's
/// address family and type. AcceptEx requires a pre-created accept socket.
SOCKET createPeerSocket(SOCKET listenSock)
{
    struct sockaddr_storage addr;
    int addrLen = sizeof(addr);
    if (getsockname(listenSock, (struct sockaddr*)&addr, &addrLen) != 0)
        return INVALID_SOCKET;
    int type;
    int typeLen = sizeof(type);
    if (getsockopt(listenSock, SOL_SOCKET, SO_TYPE, (char*)&type, &typeLen) != 0)
        return INVALID_SOCKET;
    return socket(addr.ss_family, type, 0);
}
