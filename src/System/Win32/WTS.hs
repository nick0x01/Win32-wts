module System.Win32.WTS
  ( disconnectSession
  , enumerateSessions
  , querySessionProtocol
  , waitSystemEvent
  , waitSystemEvent'
  -- * Reexports
  , wTS_CURRENT_SERVER
  , wTS_CURRENT_SERVER_HANDLE
  , SID
  , WtsConnectState (..)
  , WtsProtocolType (..)
  , WtsSessionInfo (..)
  ) where

import Foreign
import Foreign.C.Types
import Foreign.Marshal.Array
import System.Win32.WTS.Internal
import System.Win32.WTS.Types
import System.Win32.WTS.WTS_EVENTS as E
import System.Win32.Types
import qualified Data.Traversable as T

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
    withForeignPtr pSessionInfo $ \ptr ->
      peekArray (fromIntegral count) ptr >>= T.mapM convertWtsSessionInfo
  where
    rESERVED = 0
    wTS_SESSION_INFO_VER_1 = 1

-- | Disconnects the logged-on user from the specified Remote Desktop Services
-- session without closing the session.
disconnectSession :: HANDLE -> DWORD -> BOOL -> IO ()
disconnectSession h sid wait = failIfFalse_ "WTSDisconnectSession" $
  c_WTSDisconnectSession h sid wait

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
