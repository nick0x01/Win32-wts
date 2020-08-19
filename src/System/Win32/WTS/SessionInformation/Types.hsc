module System.Win32.WTS.SessionInformation.Types where

import Control.Applicative
import Data.Monoid ((<>))
import Foreign
import Foreign.C
import Foreign.C.String
import Foreign.Marshal.Array
import Foreign.Storable ()
import System.Win32.Types
import System.Win32.WTS.Types

#include <windows.h>
#include <wtsapi32.h>

-- WTS_INFO_CLASS
-- | Contains values that indicate the type of session information
-- to retrieve in a call to the WTSQuerySessionInformation function.
newtype WTS_INFO_CLASS = WTS_INFO_CLASS { infoClass :: #{type WTS_INFO_CLASS} }
  deriving (Eq)

instance Storable WTS_INFO_CLASS where
  sizeOf _ = #{size WTS_INFO_CLASS}
  alignment _ = #{alignment WTS_INFO_CLASS}
  peek p = fmap WTS_INFO_CLASS (peek $ castPtr p)
  poke p (WTS_INFO_CLASS x) = poke (castPtr p) x

#{enum WTS_INFO_CLASS, WTS_INFO_CLASS
  , icWTSInitialProgram = WTSInitialProgram
  , icWTSApplicationName = WTSApplicationName
  , icWTSWorkingDirectory = WTSWorkingDirectory
  , icWTSOEMId = WTSOEMId
  , icWTSSessionId = WTSSessionId
  , icWTSUserName = WTSUserName
  , icWTSWinStationName = WTSWinStationName
  , icWTSDomainName = WTSDomainName
  , icWTSConnectState = WTSConnectState
  , icWTSClientBuildNumber = WTSClientBuildNumber
  , icWTSClientName = WTSClientName
  , icWTSClientDirectory = WTSClientDirectory
  , icWTSClientProductId = WTSClientProductId
  , icWTSClientHardwareId = WTSClientHardwareId
  , icWTSClientAddress = WTSClientAddress
  , icWTSClientDisplay = WTSClientDisplay
  , icWTSClientProtocolType = WTSClientProtocolType
  , icWTSIdleTime = WTSIdleTime
  , icWTSLogonTime = WTSLogonTime
  , icWTSIncomingBytes = WTSIncomingBytes
  , icWTSOutgoingBytes = WTSOutgoingBytes
  , icWTSIncomingFrames = WTSIncomingFrames
  , icWTSOutgoingFrames = WTSOutgoingFrames
  , icWTSClientInfo = WTSClientInfo
  , icWTSSessionInfo = WTSSessionInfo
}
-- , icWTSSessionInfoEx = WTSSessionInfoEx
-- , icWTSConfigInfo = WTSConfigInfo
-- , icWTSValidationInfo = WTSValidationInfo
-- , icWTSSessionAddressV4 = WTSSessionAddressV4
-- , icWTSIsRemoteSession = WTSIsRemoteSession

data WtsInfoClass
  = WTSInitialProgram
  | WTSApplicationName
  | WTSWorkingDirectory
  | WTSOEMId
  | WTSSessionId
  | WTSUserName
  | WTSWinStationName
  | WTSDomainName
  | WTSConnectState
  | WTSClientBuildNumber
  | WTSClientName
  | WTSClientDirectory
  | WTSClientProductId
  | WTSClientHardwareId
  | WTSClientAddress
  | WTSClientDisplay
  | WTSClientProtocolType
  | WTSIdleTime
  | WTSLogonTime
  | WTSIncomingBytes
  | WTSOutgoingBytes
  | WTSIncomingFrames
  | WTSOutgoingFrames
  | WTSClientInfo
  | WTSSessionInfo
  -- | WTSSessionInfoEx
  -- | WTSConfigInfo
  -- | WTSValidationInfo
  -- | WTSSessionAddressV4
  -- | WTSIsRemoteSession
  deriving (Enum, Eq, Show)

-- Contains information about a Remote Desktop Connection (RDC) client.
data WTSCLIENT = WTSCLIENT
  { wtsclientClientName           :: String
  , wtsclientDomain               :: String
  , wtsclientUserName             :: String
  , wtsclientWorkDirectory        :: String
  , wtsclientInitialProgram       :: String
  , wtsclientEncryptionLevel      :: BYTE
  , wtsclientClientAddressFamily  :: ULONG
  , wtsclientClientAddress        :: [USHORT]
  , wtsclientHRes                 :: USHORT
  , wtsclientVRes                 :: USHORT
  , wtsclientColorDepth           :: USHORT
  , wtsclientClientDirectory      :: String
  , wtsclientClientBuildNumber    :: ULONG
  , wtsclientClientHardwareId     :: ULONG
  , wtsclientClientProductId      :: USHORT
  , wtsclientOutBufCountHost      :: USHORT
  , wtsclientOutBufCountClient    :: USHORT
  , wtsclientOutBufLength         :: USHORT
  , wtsclientDeviceId             :: String
  } deriving (Show)

instance Storable WTSCLIENT where
  sizeOf _ = #{size WTSCLIENTW}
  alignment _ = #{alignment WTSCLIENTW}
  peek ptr = WTSCLIENT
    <$> peekCWString (castPtr ptr)          -- WCHAR  ClientName[CLIENTNAME_LENGTH + 1];
    <*> peekCWString (ptr `plusPtr` 42)     -- WCHAR  Domain[DOMAIN_LENGTH + 1];
    <*> peekCWString (ptr `plusPtr` 78)     -- WCHAR  UserName[USERNAME_LENGTH + 1];
    <*> peekCWString (ptr `plusPtr` 120)    -- WCHAR  WorkDirectory[MAX_PATH + 1];
    <*> peekCWString (ptr `plusPtr` 642)    -- WCHAR  InitialProgram[MAX_PATH + 1];
    <*> peekByteOff ptr 1164                -- BYTE   EncryptionLevel;
    <*> peekByteOff ptr 1168                -- ULONG  ClientAddressFamily;
    <*> peekArray (#{const CLIENTADDRESS_LENGTH} + 1) (ptr `plusPtr` 1172) -- USHORT ClientAddress[CLIENTADDRESS_LENGTH + 1];
    <*> peekByteOff ptr 1234                -- USHORT HRes;
    <*> peekByteOff ptr 1236                -- USHORT VRes;
    <*> peekByteOff ptr 1238                -- USHORT ColorDepth;
    <*> peekCWString (ptr `plusPtr` 1240)   -- WCHAR  ClientDirectory[MAX_PATH + 1];
    <*> peekByteOff ptr 1764                -- ULONG  ClientBuildNumber;
    <*> peekByteOff ptr 1768                -- ULONG  ClientHardwareId;
    <*> peekByteOff ptr 1772                -- USHORT ClientProductId;
    <*> peekByteOff ptr 1774                -- USHORT OutBufCountHost;
    <*> peekByteOff ptr 1776                -- USHORT OutBufCountClient;
    <*> peekByteOff ptr 1778                -- USHORT OutBufLength;
    <*> peekCWString (ptr `plusPtr` 1780)   -- WCHAR  DeviceId[MAX_PATH + 1];
  poke _ _ =
    error "The poke function of Storable class is not implemented for WTSCLIENT"

type PWTSCLIENT = Ptr WTSCLIENT

-- Contains information about a Remote Desktop Services session.
data WTSINFO = WTSINFO
  { wtsInfState                   :: WTS_CONNECTSTATE_CLASS
  , wtsInfSessionId               :: DWORD
  , wtsInfIncomingBytes           :: DWORD
  , wtsInfOutgoingBytes           :: DWORD
  , wtsInfIncomingFrames          :: DWORD
  , wtsInfOutgoingFrames          :: DWORD
  , wtsInfIncomingCompressedBytes :: DWORD
  , wtsInfOutgoingCompressedBytes :: DWORD
  , wtsInfWinStationName          :: String
  , wtsInfDomain                  :: String
  , wtsInfUserName                :: String
  , wtsInfConnectTime             :: LARGE_INTEGER_STRUCT
  , wtsInfDisconnectTime          :: LARGE_INTEGER_STRUCT
  , wtsInfLastInputTime           :: LARGE_INTEGER_STRUCT
  , wtsInfLogonTime               :: LARGE_INTEGER_STRUCT
  , wtsInfCurrentTime             :: LARGE_INTEGER_STRUCT
  }

type PWTSINFO = Ptr WTSINFO

instance Storable WTSINFO where
  sizeOf _ = 216
  alignment _ = alignment (undefined :: CInt)
  peek ptr = WTSINFO
    <$> peekByteOff ptr 0                 -- WTS_CONNECTSTATE_CLASS State;
    <*> peekByteOff ptr 4                 -- DWORD SessionId;
    <*> peekByteOff ptr 8                 -- DWORD IncomingBytes;
    <*> peekByteOff ptr 12                -- DWORD OutgoingBytes;
    <*> peekByteOff ptr 16                -- DWORD IncomingFrames;
    <*> peekByteOff ptr 20                -- DWORD OutgoingFrames;
    <*> peekByteOff ptr 24                -- DWORD IncomingCompressedBytes;
    <*> peekByteOff ptr 28                -- DWORD OutgoingCompressedBytes;
    <*> peekCWString (ptr `plusPtr` 32)   -- WCHAR WinStationName[WINSTATIONNAME_LENGTH];
    <*> peekCWString (ptr `plusPtr` 96)   -- WCHAR Domain[DOMAIN_LENGTH];
    <*> peekCWString (ptr `plusPtr` 130)  -- WCHAR UserName[USERNAME_LENGTH+1];
    <*> peekByteOff ptr 176               -- LARGE_INTEGER ConnectTime;
    <*> peekByteOff ptr 184               -- LARGE_INTEGER DisconnectTime;
    <*> peekByteOff ptr 192               -- LARGE_INTEGER LastInputTime;
    <*> peekByteOff ptr 200               -- LARGE_INTEGER LogonTime;
    <*> peekByteOff ptr 208               -- LARGE_INTEGER CurrentTime;
  poke _ _ =
    error "The poke function of Storable class is not implemented for WTSINFO"

data WTS_CLIENT_DISPLAY = WTS_CLIENT_DISPLAY
  { wtsClientDisplayHRes        :: DWORD
  , wtsClientDisplayVRes        :: DWORD
  , wtsClientDisplayColorDepth  :: DWORD
  }

instance Show WTS_CLIENT_DISPLAY where
  show (WTS_CLIENT_DISPLAY h v cd) =
    show h <> " x " <> show v <> " " <> show cd <> "-bit"

instance Storable WTS_CLIENT_DISPLAY where
  sizeOf _ = #{size WTS_CLIENT_DISPLAY}
  alignment _ = #{alignment WTS_CLIENT_DISPLAY}
  peek p = WTS_CLIENT_DISPLAY
    <$> #{peek WTS_CLIENT_DISPLAY, HorizontalResolution} p
    <*> #{peek WTS_CLIENT_DISPLAY, VerticalResolution} p
    <*> #{peek WTS_CLIENT_DISPLAY, ColorDepth} p
  poke p x = do
    #{poke WTS_CLIENT_DISPLAY, HorizontalResolution} p $ wtsClientDisplayHRes x
    #{poke WTS_CLIENT_DISPLAY, VerticalResolution} p $ wtsClientDisplayVRes x
    #{poke WTS_CLIENT_DISPLAY, ColorDepth} p $ wtsClientDisplayColorDepth x

type PWTS_CLIENT_DISPLAY = Ptr WTS_CLIENT_DISPLAY
