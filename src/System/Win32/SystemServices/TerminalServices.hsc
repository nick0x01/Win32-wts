{-# LANGUAGE CPP, ForeignFunctionInterface #-}

module System.Win32.SystemServices.TerminalServices
  ( disconnectSession
  , enumerateSessions
  , querySessionProtocol
  , waitSystemEvent
  , waitSystemEvent'
  , wTS_CURRENT_SERVER
  , wTS_CURRENT_SERVER_HANDLE
  -- * Reexports
  , SID
  , WtsConnectState (..)
  , WtsProtocolType (..)
  , WtsSessionInfo (..)
  ) where

#include <windows.h>
#include "Win32Wts.h"

import Foreign
import Foreign.C.Types
import Foreign.Marshal.Array
import System.Win32.SystemServices.TerminalServices.Types as TS
import System.Win32.SystemServices.TerminalServices.WTS_EVENTS as E
import System.Win32.Types
import qualified Data.Traversable as T

wTS_CURRENT_SERVER :: HANDLE
wTS_CURRENT_SERVER = nullHANDLE

wTS_CURRENT_SERVER_HANDLE :: HANDLE
wTS_CURRENT_SERVER_HANDLE = nullHANDLE

-- | Retrieves a list of sessions on a specified Remote Desktop Session Host
-- (RD Session Host) server.
enumerateSessions :: HANDLE -> IO [WtsSessionInfo]
enumerateSessions h =
  with 0 $ \pCount ->
  alloca $ \ppSessionInfo -> do
    failIfFalse_ "WTSEnumerateSessions"
      $ c_WTSEnumerateSessions h rESERVED wTS_SESSION_INFO_VER_1 ppSessionInfo pCount
    count <- peek pCount
    pSessionInfo <- peek ppSessionInfo >>= newForeignPtr wtsFreeFinaliser
    result <- withForeignPtr pSessionInfo $ \ptr ->
      peekArray (fromIntegral count) ptr >>= T.mapM convertWtsSessionInfo
    return result
  where
    rESERVED = 0
    wTS_SESSION_INFO_VER_1 = 1

-- BOOL WTSEnumerateSessions(
--   _In_   HANDLE            hServer,
--   _In_   DWORD             Reserved,
--   _In_   DWORD             Version,
--   _Out_  PWTS_SESSION_INFO *ppSessionInfo,
--   _Out_  DWORD             *pCount
-- );
foreign import WINDOWS_CCONV unsafe "wtsapi32.h WTSEnumerateSessionsW"
  c_WTSEnumerateSessions :: HANDLE -> DWORD -> DWORD -> Ptr LPWTS_SESSION_INFO -> Ptr DWORD -> IO BOOL

-- | Disconnects the logged-on user from the specified Remote Desktop Services
-- session without closing the session.
disconnectSession :: HANDLE -> DWORD -> BOOL -> IO ()
disconnectSession h sid wait = failIfFalse_ "WTSDisconnectSession" $
  c_WTSDisconnectSession h sid wait

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

querySessionInformation :: HANDLE -> DWORD -> WtsInfoClass -> (LPWSTR -> DWORD -> IO b) -> IO b
querySessionInformation h sid infoClass convertFn =
  with 0 $ \pBytesReturned ->
  alloca $ \ppBuffer -> do
    failIfFalse_ "WTSQuerySessionInformation"
      $ c_WTSQuerySessionInformation h sid infoClass' ppBuffer pBytesReturned
    bytesReturned <- peek pBytesReturned
    pBuffer <- peek ppBuffer >>= newForeignPtr wtsFreeFinaliser
    withForeignPtr pBuffer $ \ptr -> convertFn ptr bytesReturned
  where
    infoClass' = WTS_INFO_CLASS (fromIntegral $ fromEnum infoClass)

-- | Retrieves protocol type for the specified session on the specified Remote
-- Desktop Session Host (RD Session Host) server.
-- It's not a Win32Api function and It returns not a Win32Api type.
querySessionProtocol :: HANDLE -> DWORD -> IO WtsProtocolType
querySessionProtocol h sid = with 0 $ \pProtoType -> do
  failIfFalse_ "querySessionProtocol"
    $ c_querySessionProtocol h sid pProtoType
  protoType <- peek pProtoType
  return . toEnum $ fromIntegral protoType

foreign import ccall "Win32Wts.h querySessionProtocol"
  c_querySessionProtocol :: HANDLE -> DWORD -> Ptr USHORT -> IO BOOL

-- BOOL WTSWaitSystemEvent(
--   IN HANDLE hServer,
--   IN DWORD  EventMask,
--   OUT DWORD *pEventFlags
-- );
foreign import WINDOWS_CCONV unsafe "wtsapi32.h WTSWaitSystemEvent"
  c_WTSWaitSystemEvent :: HANDLE -> DWORD -> Ptr DWORD -> IO BOOL

-- Call of this function (from other threads) can block the main thread
waitSystemEvent :: HANDLE -> [WTS_EVENT] -> IO [WTS_EVENT]
waitSystemEvent h eventMask =
  with 0 $ \pEventFlags -> do
    failIfFalse_ "WTSWaitSystemEvent"
      $ c_WTSWaitSystemEvent h (E.flag eventMask) pEventFlags
    E.peekWtsEvents pEventFlags

waitSystemEvent' :: HANDLE -> [WTS_EVENT] -> IO (Either ErrCode [WTS_EVENT])
waitSystemEvent' h eventMask =
  with 0 $ \pEventFlags -> do
    res <- c_WTSWaitSystemEvent h (E.flag eventMask) pEventFlags
    if res
      then Right <$> E.peekWtsEvents pEventFlags
      else Left <$> getLastError

foreign import WINDOWS_CCONV unsafe "wtsapi32.h WTSFreeMemory"
  c_WTSFreeMemory :: LPVOID -> IO ()

foreign import WINDOWS_CCONV unsafe "wtsapi32.h &WTSFreeMemory"
  wtsFreeFinaliser :: FunPtr (Ptr a -> IO ())
