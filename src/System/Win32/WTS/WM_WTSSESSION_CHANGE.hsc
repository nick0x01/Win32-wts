-- |
-- module: System.Win32.WTS.WM_WTSSESSION_CHANGE
-- portability: Windows
--
-- This module provides WM_WTSSESSION_CHANGE message type.
-- https://docs.microsoft.com/en-us/windows/win32/termserv/wm-wtssession-change

module System.Win32.WTS.WM_WTSSESSION_CHANGE
  ( WM_WTSSESSION_CHANGE (..)
  , fromDWORD
  ) where

import System.Win32.Types
import Text.Printf

#include <windows.h>
#include <wtsapi32.h>

data WM_WTSSESSION_CHANGE
  = WTS_CONSOLE_CONNECT
  -- ^ The session was connected to the console terminal or RemoteFX session.
  | WTS_CONSOLE_DISCONNECT
  -- ^ The session was disconnected from the console terminal or RemoteFX session.
  | WTS_REMOTE_CONNECT
  -- ^ The session was connected to the remote terminal.
  | WTS_REMOTE_DISCONNECT
  -- ^ The session was disconnected from the remote terminal.
  | WTS_SESSION_LOGON
  -- ^ A user has logged on to the session.
  | WTS_SESSION_LOGOFF
  -- ^ A user has logged off the session.
  | WTS_SESSION_LOCK
  -- ^ The session has been locked.
  | WTS_SESSION_UNLOCK
  -- ^ The session has been unlocked.
  | WTS_SESSION_REMOTE_CONTROL
  -- ^ The session has changed its remote controlled status.
  | WTS_SESSION_CREATE
  -- ^ Reserved for future use.
  | WTS_SESSION_TERMINATE
  -- ^ Reserved for future use.
  deriving (Eq, Show)

fromDWORD :: DWORD -> Either String WM_WTSSESSION_CHANGE
fromDWORD #{const WTS_CONSOLE_CONNECT}        = Right WTS_CONSOLE_CONNECT
fromDWORD #{const WTS_CONSOLE_DISCONNECT}     = Right WTS_CONSOLE_DISCONNECT
fromDWORD #{const WTS_REMOTE_CONNECT}         = Right WTS_REMOTE_CONNECT
fromDWORD #{const WTS_REMOTE_DISCONNECT}      = Right WTS_REMOTE_DISCONNECT
fromDWORD #{const WTS_SESSION_LOGON}          = Right WTS_SESSION_LOGON
fromDWORD #{const WTS_SESSION_LOGOFF}         = Right WTS_SESSION_LOGOFF
fromDWORD #{const WTS_SESSION_LOCK}           = Right WTS_SESSION_LOCK
fromDWORD #{const WTS_SESSION_UNLOCK}         = Right WTS_SESSION_UNLOCK
fromDWORD #{const WTS_SESSION_REMOTE_CONTROL} = Right WTS_SESSION_REMOTE_CONTROL
fromDWORD #{const WTS_SESSION_CREATE}         = Right WTS_SESSION_CREATE
fromDWORD #{const WTS_SESSION_TERMINATE}      = Right WTS_SESSION_TERMINATE
fromDWORD x = Left $ "The " ++ printf "%x" x ++ " session change message code is unsupported by this binding."
