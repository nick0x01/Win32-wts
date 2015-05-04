module System.Win32.SystemServices.TerminalServices.Types where

#include <windows.h>
#include <wtsapi32.h>
#include <Win32Wts.h>

import Control.Applicative
import Foreign
import Foreign.C
import Foreign.Storable ()
import System.Win32.Types

-- | Specifies the connection state of a Remote Desktop Services session.
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

-- | Specifies information about the protocol type for the session.
data WtsProtocolType
  -- | The console session.
  = WtsConsole
  -- | This value is retained for legacy purposes.
  | WtsICA
  -- | The RDP protocol.
  | WtsRDP
  deriving (Eq, Show, Enum)

-- | Contains information about a client session on a Remote Desktop Session
-- Host (RD Session Host) server.
data WTS_SESSION_INFO = WTS_SESSION_INFO
  { sessionId      :: DWORD
  -- ^ Session identifier of the session.
  , winStationName :: LPWSTR
  -- ^ Pointer to a null-terminated string that contains the WinStation name
  -- of this session. The WinStation name is a name that Windows associates
  -- with the session, for example, "services", "console", or "RDP-Tcp#0".
  , state          :: WTS_CONNECTSTATE_CLASS
  -- ^ A value from the WTS_CONNECTSTATE_CLASS enumeration type that indicates
  -- the session's current connection state.
  }

instance Storable WTS_SESSION_INFO where
  sizeOf _ = #{size WTS_SESSION_INFOW}
  alignment _ = alignment (undefined :: CInt)
  peek ptr = WTS_SESSION_INFO
    <$> (peek . castPtr) ptr
    <*> (peek $ castPtr ptr `plusPtr` 4)
    <*> (peek $ castPtr ptr `plusPtr` 8)
  poke ptr (WTS_SESSION_INFO sid wsn st) = do
    poke (castPtr ptr) sid
    poke (castPtr ptr `plusPtr` 4) wsn
    poke (castPtr ptr `plusPtr` 8) st

type LPWTS_SESSION_INFO = Ptr WTS_SESSION_INFO
