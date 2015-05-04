#include "Win32Wts.h"
#include <wtsapi32.h>

BOOL querySessionProtocol(HANDLE hServer, DWORD sessionId, USHORT *pProtocolType)
{
  LPTSTR pBuffer = NULL;
  DWORD bytesReturned = 0;
  if (WTSQuerySessionInformation(hServer, sessionId, WTSClientProtocolType, &pBuffer, &bytesReturned))
  {
    *pProtocolType = *(USHORT*)pBuffer;
    WTSFreeMemory(pBuffer);
    return TRUE;
  }
  else
  {
    return FALSE;
  }
}
