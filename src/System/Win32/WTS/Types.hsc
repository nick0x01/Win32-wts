{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RecordWildCards #-}
module System.Win32.WTS.Types where

import Control.Applicative
import Foreign
import Foreign.C
import Foreign.Storable ()
import System.Win32.Types

#include <windows.h>
#include <wtsapi32.h>

wTS_CURRENT_SERVER :: HANDLE
wTS_CURRENT_SERVER = nullHANDLE

wTS_CURRENT_SERVER_HANDLE :: HANDLE
wTS_CURRENT_SERVER_HANDLE = nullHANDLE

wTS_CURRENT_SESSION :: DWORD
wTS_CURRENT_SESSION = #{const WTS_CURRENT_SESSION}

type SID = DWORD

type ULONG = Word32

wcharSize :: Int
wcharSize = #{size wchar_t}

newtype WTS_CONNECTSTATE_CLASS = WTS_CONNECTSTATE_CLASS { connStateClass :: #{type WTS_CONNECTSTATE_CLASS} }
  deriving (Eq)

instance Storable WTS_CONNECTSTATE_CLASS where
  sizeOf _ = #{size WTS_CONNECTSTATE_CLASS}
  alignment _ = 4
  peek p = fmap WTS_CONNECTSTATE_CLASS (peek $ castPtr p)
  poke p (WTS_CONNECTSTATE_CLASS v) = poke (castPtr p) v

#{enum WTS_CONNECTSTATE_CLASS, WTS_CONNECTSTATE_CLASS
 , csActive = WTSActive
 , csConnected = WTSConnected
 , csConnectQuery = WTSConnectQuery
 , csShadow = WTSShadow
 , csDisconnected = WTSDisconnected
 , csIdle = WTSIdle
 , csListen = WTSListen
 , csReset = WTSReset
 , csDown = WTSDown
 , csInit  = WTSInit
}

-- | Specifies the connection state of a Remote Desktop Services session.
data WtsConnectState
  = WTSActive
  | WTSConnected
  | WTSConnectQuery
  | WTSShadow
  | WTSDisconnected
  | WTSIdle
  | WTSListen
  | WTSReset
  | WTSDown
  | WTSInit
  deriving (Enum, Eq, Show)

convertWtsConnectionState :: WTS_CONNECTSTATE_CLASS -> WtsConnectState
convertWtsConnectionState = toEnum . fromIntegral . connStateClass

-- | Specifies information about the protocol type for the session.
data WtsProtocolType
  -- | The console session.
  = WtsConsole
  -- | This value is retained for legacy purposes.
  | WtsICA
  -- | The RDP protocol.
  | WtsRDP
  deriving (Enum, Eq, Show)

convertWtsProtocolType :: USHORT -> WtsProtocolType
convertWtsProtocolType = toEnum . fromIntegral

-- | Contains information about a client session on a Remote Desktop Session
-- Host (RD Session Host) server.
data WtsSessionInfo = WtsSessionInfo
  { wsiSessionId      :: DWORD
  -- ^ Session identifier of the session.
  , wsiWinStationName :: String
  -- ^ WinStation name of this session. The WinStation name is a name
  -- that Windows associates with the session, for example, "services",
  -- "console", or "RDP-Tcp#0".
  , wsiState          :: WtsConnectState
  -- ^ A value indicates the session's current connection state.
  } deriving (Eq, Show)

convertWtsSessionInfo :: WTS_SESSION_INFO -> IO WtsSessionInfo
convertWtsSessionInfo (WTS_SESSION_INFO sid wsName st) = WtsSessionInfo
    <$> pure sid
    <*> peekCWString wsName
    <*> pure (convertContentState $ connStateClass st)
  where convertContentState = toEnum . fromIntegral

data WTS_SESSION_INFO = WTS_SESSION_INFO
  { sessionId      :: DWORD
  , winStationName :: LPWSTR
  , state          :: WTS_CONNECTSTATE_CLASS
  }

instance Storable WTS_SESSION_INFO where
  sizeOf _ = #{size WTS_SESSION_INFOW}
  alignment _ = #{alignment WTS_SESSION_INFOW}
  peek p = WTS_SESSION_INFO
    <$> #{peek WTS_SESSION_INFOW, SessionId} p
    <*> #{peek WTS_SESSION_INFOW, pWinStationName} p
    <*> #{peek WTS_SESSION_INFOW, State} p
  poke p x = do
    #{poke WTS_SESSION_INFOW, SessionId} p $ sessionId x
    #{poke WTS_SESSION_INFOW, pWinStationName} p $ winStationName x
    #{poke WTS_SESSION_INFOW, State} p $ state x

type PWTS_SESSION_INFO = Ptr WTS_SESSION_INFO

data LARGE_INTEGER_STRUCT = LARGE_INTEGER_STRUCT
  { largeIntQuadPart :: LARGE_INTEGER
  } deriving (Show)

instance Storable LARGE_INTEGER_STRUCT where
  sizeOf _ = #{size LARGE_INTEGER}
  alignment _ = #{alignment LARGE_INTEGER}
  peek p = LARGE_INTEGER_STRUCT <$> #{peek LARGE_INTEGER, QuadPart} p
  poke p x = #{poke LARGE_INTEGER, QuadPart} p $ largeIntQuadPart x

newtype MessageResponse = MessageResponse { unMessageResponse :: DWORD }
  deriving (Show, Eq, Storable)

pattern IDABORT = MessageResponse #{const IDABORT}
pattern IDCANCEL = MessageResponse #{const IDCANCEL}
pattern IDCONTINUE = MessageResponse #{const IDCONTINUE}
pattern IDIGNORE = MessageResponse #{const IDIGNORE}
pattern IDNO = MessageResponse #{const IDNO}
pattern IDOK = MessageResponse #{const IDOK}
pattern IDRETRY = MessageResponse #{const IDRETRY}
pattern IDTRYAGAIN = MessageResponse #{const IDTRYAGAIN}
pattern IDYES = MessageResponse #{const IDYES}
-- The bWait parameter was FALSE, so the function returned without waiting for a response.
pattern IDASYNC = MessageResponse #{const IDASYNC}
-- The bWait parameter was TRUE and the time-out interval elapsed.
pattern IDTIMEOUT = MessageResponse #{const IDTIMEOUT}

newtype MessageBoxStyle = MessageBoxStyle { unMessageBoxStyle :: DWORD }
  deriving (Show, Eq, Storable)

-- The message box contains three push buttons: Abort, Retry, and Ignore.
pattern MB_ABORTRETRYIGNORE = MessageBoxStyle #{const MB_ABORTRETRYIGNORE}
-- The message box contains three push buttons: Cancel, Try Again, Continue. Use this message box type instead of MB_ABORTRETRYIGNORE.
pattern MB_CANCELTRYCONTINUE = MessageBoxStyle #{const MB_CANCELTRYCONTINUE}
-- The message box contains one push button: OK. This is the default.
pattern MB_OK = MessageBoxStyle #{const MB_OK}
-- The message box contains two push buttons: OK and Cancel.
pattern MB_OKCANCEL = MessageBoxStyle #{const MB_OKCANCEL}
-- The message box contains two push buttons: Retry and Cancel.
pattern MB_RETRYCANCEL = MessageBoxStyle #{const MB_RETRYCANCEL}
-- The message box contains two push buttons: Yes and No.
pattern MB_YESNO = MessageBoxStyle #{const MB_YESNO}
-- The message box contains three push buttons: Yes, No, and Cancel.
pattern MB_YESNOCANCEL = MessageBoxStyle #{const MB_YESNOCANCEL}
