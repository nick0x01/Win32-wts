{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
module System.Win32.WTS
  ( disconnectSession
  , logoffSession
  , enumerateSessions
  , waitSystemEvent
  , waitSystemEvent'
  , getActiveConsoleSessionId
  , isConcoleSession
  , sendMessage
  -- * Reexports
  , wTS_CURRENT_SERVER
  , wTS_CURRENT_SERVER_HANDLE
  , wTS_CURRENT_SESSION
  , SID
  , WtsConnectState (..)
  , WtsProtocolType (..)
  , WtsSessionInfo (..)
  , MessageResponse (..)
  , pattern IDABORT
  , pattern IDCANCEL
  , pattern IDCONTINUE
  , pattern IDIGNORE
  , pattern IDNO
  , pattern IDOK
  , pattern IDRETRY
  , pattern IDTRYAGAIN
  , pattern IDYES
  , pattern IDASYNC
  , pattern IDTIMEOUT
  , MessageBoxStyle (..)
  , pattern MB_ABORTRETRYIGNORE
  , pattern MB_CANCELTRYCONTINUE
  , pattern MB_OK
  , pattern MB_OKCANCEL
  , pattern MB_RETRYCANCEL
  , pattern MB_YESNO
  , pattern MB_YESNOCANCEL
  ) where

import Data.Text (Text)
import Foreign
import Foreign.C.Types
import Foreign.C.String
import Foreign.Marshal.Array
import System.Win32.WTS.Internal
import System.Win32.WTS.Types
import System.Win32.WTS.WTS_EVENTS as E
import System.Win32.Types
import qualified Data.Traversable as T
import qualified Data.Text as T
import qualified System.Win32.Error.Foreign as Err

-- | Retrieves a list of sessions on a specified Remote Desktop Session Host
-- (RD Session Host) server.
-- https://docs.microsoft.com/en-us/windows/win32/api/wtsapi32/nf-wtsapi32-wtsenumeratesessionsw
enumerateSessions :: HANDLE -> IO [WtsSessionInfo]
enumerateSessions h =
  with 0 $ \pCount ->
  alloca $ \ppSessionInfo -> do
    Err.failIfFalse_ "WTSEnumerateSessions"
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
-- https://docs.microsoft.com/en-us/windows/win32/api/wtsapi32/nf-wtsapi32-wtsdisconnectsession
disconnectSession :: HANDLE -> SID -> BOOL -> IO ()
disconnectSession h sid wait = Err.failIfFalse_ "WTSDisconnectSession" $
  c_WTSDisconnectSession h sid wait

-- | Logs off a specified Remote Desktop Services session.
-- https://docs.microsoft.com/en-us/windows/win32/api/wtsapi32/nf-wtsapi32-wtslogoffsession
logoffSession :: HANDLE -> SID -> BOOL -> IO ()
logoffSession h sid wait = Err.failIfFalse_ "WTSLogoffSession" $
  c_WTSLogoffSession h sid wait

-- | (!) Call of this function (from other threads) can block the main thread.
-- Waits for a Remote Desktop Services event before returning to the caller.
-- https://docs.microsoft.com/en-us/windows/win32/api/wtsapi32/nf-wtsapi32-wtswaitsystemevent
waitSystemEvent :: HANDLE -> [WTS_EVENT] -> IO [WTS_EVENT]
waitSystemEvent h eventMask =
  with 0 $ \pEventFlags -> do
    Err.failIfFalse_ "WTSWaitSystemEvent"
      $ c_WTSWaitSystemEvent h (E.flag eventMask) pEventFlags
    E.peekWtsEvents pEventFlags

waitSystemEvent' :: HANDLE -> [WTS_EVENT] -> IO (Either ErrCode [WTS_EVENT])
waitSystemEvent' h eventMask =
  with 0 $ \pEventFlags -> do
    res <- c_WTSWaitSystemEvent h (E.flag eventMask) pEventFlags
    if res
      then Right <$> E.peekWtsEvents pEventFlags
      else Left <$> getLastError

-- | Retrieves the session identifier of the console session. The console session is
-- the session that is currently attached to the physical console.
-- Note that it is not necessary that Remote Desktop Services be running for this function to succeed.
-- https://docs.microsoft.com/en-us/windows/win32/api/winbase/nf-winbase-wtsgetactiveconsolesessionid
getActiveConsoleSessionId :: IO (Maybe SID)
getActiveConsoleSessionId = do
  sid <- c_WTSGetActiveConsoleSessionId
  if sid == noSessionAttached
    then return Nothing
    else return (Just sid)
  where
    noSessionAttached = 0xFFFFFFFF

isConcoleSession :: SID -> IO Bool
isConcoleSession 0 = return False
isConcoleSession sid =
  let isConsole x = x == sid
  in  maybe False isConsole <$> getActiveConsoleSessionId

-- | Displays a message box on the client desktop of a specified Remote Desktop Services session.
-- https://docs.microsoft.com/en-us/windows/win32/api/wtsapi32/nf-wtsapi32-wtssendmessagew
sendMessage :: HANDLE -> SID -> Text -> Text -> MessageBoxStyle -> DWORD -> Bool -> IO MessageResponse
sendMessage h sid title message (MessageBoxStyle style) timeout wait =
  withCWStringLen (T.unpack title) $ \(pTitle, titleLen) ->
  withCWStringLen (T.unpack message) $ \(pMessage, messageLen) ->
  with 0 $ \pResponse -> do
    let titleSize = fromIntegral (titleLen * wcharSize)
        messageSize = fromIntegral (messageLen * wcharSize)
    Err.failIfFalse_ "WTSSendMessage" $
      c_WTSSendMessage h sid pTitle titleSize pMessage messageSize style timeout pResponse wait
    MessageResponse <$> peek pResponse
