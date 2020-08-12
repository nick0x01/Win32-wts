{-# LANGUAGE CPP, ForeignFunctionInterface #-}
module System.Win32.WTS.Internal where

import Foreign
import Foreign.C.Types
import System.Win32.WTS.Types
import System.Win32.Types

#include <windows.h>

-- BOOL WTSEnumerateSessions(
--   _In_   HANDLE            hServer,
--   _In_   DWORD             Reserved,
--   _In_   DWORD             Version,
--   _Out_  PWTS_SESSION_INFO *ppSessionInfo,
--   _Out_  DWORD             *pCount
-- );
foreign import WINDOWS_CCONV unsafe "wtsapi32.h WTSEnumerateSessionsW"
  c_WTSEnumerateSessions :: HANDLE -> DWORD -> DWORD -> Ptr LPWTS_SESSION_INFO -> Ptr DWORD -> IO BOOL

-- BOOL WTSDisconnectSession(
--   _In_  HANDLE hServer,
--   _In_  DWORD  SessionId,
--   _In_  BOOL   bWait
-- );
foreign import WINDOWS_CCONV unsafe "wtsapi32.h WTSDisconnectSession"
  c_WTSDisconnectSession :: HANDLE -> DWORD -> BOOL -> IO BOOL

-- BOOL WTSQuerySessionInformationW(
--   IN HANDLE         hServer,
--   IN DWORD          SessionId,
--   IN WTS_INFO_CLASS WTSInfoClass,
--   LPWSTR            *ppBuffer,
--   DWORD             *pBytesReturned
-- );
foreign import WINDOWS_CCONV unsafe "wtsapi32.h WTSQuerySessionInformationW"
  c_WTSQuerySessionInformation :: HANDLE -> DWORD -> WTS_INFO_CLASS -> Ptr LPWSTR -> Ptr DWORD -> IO BOOL

-- BOOL WTSWaitSystemEvent(
--   IN HANDLE hServer,
--   IN DWORD  EventMask,
--   OUT DWORD *pEventFlags
-- );
foreign import WINDOWS_CCONV unsafe "wtsapi32.h WTSWaitSystemEvent"
  c_WTSWaitSystemEvent :: HANDLE -> DWORD -> Ptr DWORD -> IO BOOL

foreign import WINDOWS_CCONV unsafe "wtsapi32.h WTSFreeMemory"
  c_WTSFreeMemory :: LPVOID -> IO ()

foreign import WINDOWS_CCONV unsafe "wtsapi32.h &WTSFreeMemory"
  wtsFreeFinaliser :: FunPtr (Ptr a -> IO ())
