module System.Win32.SystemServices.TerminalServices.Types where

#include <windows.h>
#include <wtsapi32.h>
#include <Win32Wts.h>

import Control.Applicative
import Foreign
import Foreign.C
import Foreign.C.String
import Foreign.Storable ()
import System.Win32.Types

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

-- | Specifies information about the protocol type for the session.
data WtsProtocolType
  -- | The console session.
  = WtsConsole
  -- | This value is retained for legacy purposes.
  | WtsICA
  -- | The RDP protocol.
  | WtsRDP
  deriving (Enum, Eq, Show)

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
  alignment _ = alignment (undefined :: CInt)
  peek p = WTS_SESSION_INFO
    <$> #{peek WTS_SESSION_INFO, SessionId} p
    <*> #{peek WTS_SESSION_INFO, pWinStationName} p
    <*> #{peek WTS_SESSION_INFO, State} p
  poke p x = do
    #{poke WTS_SESSION_INFO, SessionId} p $ sessionId x
    #{poke WTS_SESSION_INFO, pWinStationName} p $ winStationName x
    #{poke WTS_SESSION_INFO, State} p $ state x

type LPWTS_SESSION_INFO = Ptr WTS_SESSION_INFO
