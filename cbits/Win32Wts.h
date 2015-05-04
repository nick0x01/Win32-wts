#ifndef __HS_WIN32WTS_H
#define __HS_WIN32WTS_H

#include <windows.h>

BOOL querySessionProtocol(HANDLE hServer, DWORD sessionId, USHORT *pProtocolType);

#endif
