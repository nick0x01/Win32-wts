{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}
module System.Win32.WTS.SessionInformation
  ( querySessionInformation
  , toNothingIfNoSession
  , querySessionUserName
  , querySessionDomainName
  , queryWinStationName
  , queryClientName
  , queryClientProtocolType
  , queryClientBuildNumber
  , convertWtsConnectionState
  , queryClientInfo
  , queryClientDisplay
  , queryConnectionState
  , module Reexports
  ) where

import Control.Exception
import Foreign
import Foreign.C.String
import System.Win32.Types
import System.Win32.WTS.Internal
import System.Win32.WTS.SessionInformation.Types
import System.Win32.WTS.Types
import qualified System.Win32.Error as Err
import qualified System.Win32.Error.Foreign as Err
import qualified System.Win32.WTS.SessionInformation.Types as Reexports
  ( WTSINFO (..), WTSCLIENT (..), WTS_CLIENT_DISPLAY (..), pattern AF_INET )

-- | Retrieves session information for the specified session on the specified
-- Remote Desktop Session Host (RD Session Host) server. It can be used to query
-- session information on local and remote RD Session Host servers.
querySessionInformation :: HANDLE -> SID -> WtsInfoClass -> (LPWSTR -> DWORD -> IO b) -> IO b
querySessionInformation h sid infoClass convertFn =
  with 0 $ \pBytesReturned ->
  alloca $ \ppBuffer -> do
    Err.failIfFalse_ "WTSQuerySessionInformation"
      $ c_WTSQuerySessionInformation h sid infoClass' ppBuffer pBytesReturned
    bytesReturned <- peek pBytesReturned
    pBuffer <- peek ppBuffer >>= newForeignPtr wtsFreeFinaliser
    withForeignPtr pBuffer $ \ptr -> convertFn ptr bytesReturned
  where
    infoClass' = WTS_INFO_CLASS (fromIntegral $ fromEnum infoClass)

toNothingIfNoSession :: IO a -> IO (Maybe a)
toNothingIfNoSession act =
  Err.tryWin32 act >>= \case
    Right x -> return (Just x)
    Left er ->
      if Err.FileNotFound == Err.errCode er
        then return Nothing
        else throwIO er

querySessionStringInfo :: WtsInfoClass -> HANDLE -> SID -> IO String
querySessionStringInfo infoClass h sid =
  querySessionInformation h sid infoClass $ \ptr _ -> peekCWString ptr

querySessionUserName :: HANDLE -> SID -> IO String
querySessionUserName =
  querySessionStringInfo WTSUserName

querySessionDomainName :: HANDLE -> SID -> IO String
querySessionDomainName =
  querySessionStringInfo WTSDomainName

queryWinStationName :: HANDLE -> SID -> IO String
queryWinStationName =
  querySessionStringInfo WTSWinStationName

queryClientName :: HANDLE -> SID -> IO String
queryClientName =
  querySessionStringInfo WTSClientName

queryClientProtocolType :: HANDLE -> SID -> IO WtsProtocolType
queryClientProtocolType h sid =
  querySessionInformation h sid WTSClientProtocolType $ \ptr _ ->
    convertWtsProtocolType <$> peek (castPtr ptr)

queryClientBuildNumber :: HANDLE -> SID -> IO ULONG
queryClientBuildNumber h sid =
  querySessionInformation h sid WTSClientBuildNumber $ \ptr _ ->
    peek (castPtr ptr)

querySessionInfo :: HANDLE -> SID -> IO WTSINFO
querySessionInfo h sid =
  querySessionInformation h sid WTSSessionInfo $ \ptr _ ->
    peek (castPtr ptr)

queryClientInfo :: HANDLE -> SID -> IO WTSCLIENT
queryClientInfo h sid =
  querySessionInformation h sid WTSClientInfo $ \ptr _ ->
    peek (castPtr ptr)

queryClientDisplay :: HANDLE -> SID -> IO WTS_CLIENT_DISPLAY
queryClientDisplay h sid =
  querySessionInformation h sid WTSClientDisplay $ \ptr _ ->
    peek (castPtr ptr)

queryConnectionState :: HANDLE -> SID -> IO WtsConnectState
queryConnectionState h sid =
  querySessionInformation h sid WTSConnectState $ \ptr _ ->
    convertWtsConnectionState <$> peek (castPtr ptr)

-- queryConnectionStateMaybe :: HANDLE -> SID -> IO (Maybe WtsConnectState)
-- queryConnectionStateMaybe h sid =
--   querySessionInformationMaybe h sid WTSConnectState $ \ptr _ ->
--     convertWtsConnectionState <$> peek (castPtr ptr)
