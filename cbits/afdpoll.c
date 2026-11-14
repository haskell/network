/* Readiness on Windows through the AFD driver beneath Winsock, as libuv,
   wepoll and mio do.  IOCTL_AFD_POLL is undocumented but long stable; it
   completes like any overlapped operation on the socket. */

#include <stdint.h>
#include <string.h>
#include "HsNet.h"
#include <winternl.h>

#define IOCTL_AFD_POLL 0x00012024
#ifndef STATUS_PENDING
# define STATUS_PENDING ((NTSTATUS) 0x00000103L)
#endif
#ifndef NT_SUCCESS
# define NT_SUCCESS(st) (((NTSTATUS) (st)) >= 0)
#endif

typedef struct {
    HANDLE   Handle;
    ULONG    Events;
    NTSTATUS Status;
} AFD_POLL_HANDLE_INFO;

typedef struct {
    LARGE_INTEGER        Timeout;
    ULONG                NumberOfHandles;
    ULONG                Exclusive;
    AFD_POLL_HANDLE_INFO Handles[1];
} AFD_POLL_INFO;

typedef NTSTATUS (NTAPI *NtDeviceIoControlFile_t)(HANDLE, HANDLE, PVOID, PVOID,
    PIO_STATUS_BLOCK, ULONG, PVOID, ULONG, PVOID, ULONG);
typedef ULONG (NTAPI *RtlNtStatusToDosError_t)(NTSTATUS);

static NtDeviceIoControlFile_t pNtDeviceIoControlFile;
static RtlNtStatusToDosError_t pRtlNtStatusToDosError;

int
hsnet_afd_poll_info_size(void)
{
    return sizeof(AFD_POLL_INFO);
}

/* Starts waiting for events on s.  Answers 0 when they are there already,
   ERROR_IO_PENDING when the wait has started, or a Win32 error code. */
int
hsnet_afd_poll(unsigned long sock, ULONG events, AFD_POLL_INFO *info, LPOVERLAPPED ov)
{
    SOCKET s = (SOCKET) sock;
    if (pNtDeviceIoControlFile == NULL) {
        HMODULE ntdll = GetModuleHandleW(L"ntdll.dll");
        pRtlNtStatusToDosError = (RtlNtStatusToDosError_t)
            (void (*)(void)) GetProcAddress(ntdll, "RtlNtStatusToDosError");
        pNtDeviceIoControlFile = (NtDeviceIoControlFile_t)
            (void (*)(void)) GetProcAddress(ntdll, "NtDeviceIoControlFile");
        if (pNtDeviceIoControlFile == NULL)
            return ERROR_NOT_SUPPORTED;
    }
    memset(info, 0, sizeof(*info));
    info->Timeout.QuadPart = INT64_MAX;
    info->NumberOfHandles = 1;
    info->Exclusive = FALSE;
    info->Handles[0].Handle = (HANDLE) s;
    info->Handles[0].Events = events;
    /* As DeviceIoControl does: until the poll completes, the status block
       is left alone, and GetOverlappedResult must not read it as done. */
    ov->Internal = STATUS_PENDING;
    NTSTATUS st = pNtDeviceIoControlFile((HANDLE) s, NULL, NULL, ov,
        (PIO_STATUS_BLOCK) &ov->Internal, IOCTL_AFD_POLL,
        info, sizeof(*info), info, sizeof(*info));
    if (st == STATUS_PENDING)
        return ERROR_IO_PENDING;
    if (NT_SUCCESS(st))
        return 0;
    return pRtlNtStatusToDosError ? (int) pRtlNtStatusToDosError(st) : ERROR_GEN_FAILURE;
}

ULONG
hsnet_afd_poll_events(AFD_POLL_INFO *info)
{
    return info->Handles[0].Events;
}
