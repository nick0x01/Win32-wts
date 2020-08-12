{-# LANGUAGE ScopedTypeVariables #-}
module System.Win32.WTS.SessionInformation
  ( querySessionInformation
  , querySessionUserName
  , querySessionDomainName
  , queryWinStationName
  , queryClientName
  , queryClientProtocolType
  ) where

import Foreign
import Foreign.C.String
import System.Win32.Types
import System.Win32.WTS.Internal
import System.Win32.WTS.Types

-- | Retrieves session information for the specified session on the specified
-- Remote Desktop Session Host (RD Session Host) server. It can be used to query
-- session information on local and remote RD Session Host servers.
querySessionInformation :: HANDLE -> SID -> WtsInfoClass -> (LPWSTR -> DWORD -> IO b) -> IO b
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
  querySessionInformation h sid WTSClientProtocolType $ \ptr _ -> do
    (protoType :: USHORT) <- peek (castPtr ptr)
    return (toEnum $ fromIntegral protoType)
