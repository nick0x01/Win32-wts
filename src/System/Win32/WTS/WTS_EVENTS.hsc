module System.Win32.WTS.WTS_EVENTS
  ( WTS_EVENT (..)
  , peekWtsEvents
  , pokeWtsEvents
  , unflag
  , flag
  ) where

import Data.Bits
import Data.Maybe (mapMaybe)
import Foreign
import System.Win32.Types
import Text.Printf

#include <windows.h>
#include <wtsapi32.h>

data WTS_EVENT
  = WTS_EVENT_NONE
  | WTS_EVENT_ALL
  | WTS_EVENT_CREATE
  | WTS_EVENT_DELETE
  | WTS_EVENT_RENAME
  | WTS_EVENT_CONNECT
  | WTS_EVENT_DISCONNECT
  | WTS_EVENT_LOGON
  | WTS_EVENT_LOGOFF
  | WTS_EVENT_STATECHANGE
  | WTS_EVENT_LICENSE
  | WTS_EVENT_FLUSH
  deriving (Eq, Show)

peekWtsEvents :: Ptr DWORD -> IO [WTS_EVENT]
peekWtsEvents ptr = unflag <$> peek ptr

pokeWtsEvents :: Ptr DWORD -> [WTS_EVENT] -> IO ()
pokeWtsEvents ptr = poke ptr . flag

toDWORD :: WTS_EVENT -> DWORD
toDWORD WTS_EVENT_ALL          = #{const WTS_EVENT_ALL}
toDWORD WTS_EVENT_NONE         = #{const WTS_EVENT_NONE}
toDWORD WTS_EVENT_CREATE       = #{const WTS_EVENT_CREATE}
toDWORD WTS_EVENT_DELETE       = #{const WTS_EVENT_DELETE}
toDWORD WTS_EVENT_RENAME       = #{const WTS_EVENT_RENAME}
toDWORD WTS_EVENT_CONNECT      = #{const WTS_EVENT_CONNECT}
toDWORD WTS_EVENT_DISCONNECT   = #{const WTS_EVENT_DISCONNECT}
toDWORD WTS_EVENT_LOGON        = #{const WTS_EVENT_LOGON}
toDWORD WTS_EVENT_LOGOFF       = #{const WTS_EVENT_LOGOFF}
toDWORD WTS_EVENT_STATECHANGE  = #{const WTS_EVENT_STATECHANGE}
toDWORD WTS_EVENT_LICENSE      = #{const WTS_EVENT_LICENSE}
toDWORD WTS_EVENT_FLUSH        = #{const WTS_EVENT_FLUSH}

fromDWORD :: DWORD -> Either String WTS_EVENT
-- fromDWORD #{const WTS_EVENT_NONE} = Right WTS_EVENT_NONE
fromDWORD #{const WTS_EVENT_ALL} = Right WTS_EVENT_ALL
fromDWORD #{const WTS_EVENT_CREATE} = Right WTS_EVENT_CREATE
fromDWORD #{const WTS_EVENT_DELETE} = Right WTS_EVENT_DELETE
fromDWORD #{const WTS_EVENT_RENAME} = Right WTS_EVENT_RENAME
fromDWORD #{const WTS_EVENT_CONNECT} = Right WTS_EVENT_CONNECT
fromDWORD #{const WTS_EVENT_DISCONNECT} = Right WTS_EVENT_DISCONNECT
fromDWORD #{const WTS_EVENT_LOGON} = Right WTS_EVENT_LOGON
fromDWORD #{const WTS_EVENT_LOGOFF} = Right WTS_EVENT_LOGOFF
fromDWORD #{const WTS_EVENT_STATECHANGE} = Right WTS_EVENT_STATECHANGE
fromDWORD #{const WTS_EVENT_LICENSE} = Right WTS_EVENT_LICENSE
fromDWORD #{const WTS_EVENT_FLUSH} = Right WTS_EVENT_FLUSH
fromDWORD x = Left $ "The " ++ printf "%x" x ++ " control code is unsupported by this binding."

-- | Suppress the 'Left' value of an 'Either'
hush :: Either a b -> Maybe b
hush = either (const Nothing) Just

unflag :: DWORD -> [WTS_EVENT]
unflag f =
  if f == #{const WTS_EVENT_NONE}
    then [WTS_EVENT_NONE]
    else mapMaybe (hush . fromDWORD . (.&. f)) masks
  where
    masks = take 32 $ iterate (`shiftL` 1) 1

flag :: [WTS_EVENT] -> DWORD
flag = foldl (\flag' f -> flag' .|. toDWORD f) 0
