{-# LANGUAGE ForeignFunctionInterface #-}

module System.Win32.SystemServices.TerminalServices
  ( disconnectSession
  , enumerateSessions
  , querySessionProtocol
  , wTS_CURRENT_SERVER
  , wTS_CURRENT_SERVER_HANDLE
  -- * Reexports
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
  with (nullPtr :: (Ptr WTS_SESSION_INFO)) $ \ppBuff -> do
    failIfFalse_ "WTSEnumerateSessions"
      $ c_WTSEnumerateSessions h 0 1 ppBuff pCount
    count <- peek pCount
    pBuff <- peek ppBuff
    result <- peekArray (fromIntegral count) pBuff >>= T.mapM convertWtsSessionInfo
    c_WTSFreeMemory $ castPtr pBuff
    return result

-- BOOL WTSEnumerateSessions(
--   _In_   HANDLE            hServer,
--   _In_   DWORD             Reserved,
--   _In_   DWORD             Version,
--   _Out_  PWTS_SESSION_INFO *ppSessionInfo,
--   _Out_  DWORD             *pCount
-- );
foreign import stdcall "wtsapi32.h WTSEnumerateSessionsW"
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
foreign import stdcall "wtsapi32.h WTSDisconnectSession"
  c_WTSDisconnectSession :: HANDLE -> DWORD -> BOOL -> IO BOOL

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

foreign import stdcall "wtsapi32.h WTSFreeMemory"
  c_WTSFreeMemory :: LPVOID -> IO ()
