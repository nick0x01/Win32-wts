{-# LANGUAGE LambdaCase #-}
module Main (main) where

import Control.Monad (void)
import Data.Monoid ((<>))
import Options.Applicative
import Text.Printf
import qualified Data.Text as T

import System.Win32.WTS
import System.Win32.WTS.SessionInformation

data Command
  = SessionsList
  | Disconnect SID
  | Logoff SID
  | SendMessage SID String String
  deriving (Eq, Show)

commandParse :: Parser Command
commandParse = subparser (
       command "sessions-list" (info (pure SessionsList) (progDesc "Show all sessions at the current machine"))
    <> command "disconnect" (info disconnectCmd (progDesc "Disconnect the session with specified SID"))
    <> command "logoff" (info logoffCmd (progDesc "Logoff the sessions with specified SID"))
    <> command "message" (info messageCmd (progDesc "Send a message to the session with specified SID"))
  )
  where
    disconnectCmd = Disconnect <$> argument auto (metavar "SID" <> help "Session ID to disconnect")
    logoffCmd = Logoff <$> argument auto (metavar "SID" <> help "Session ID to logoff")
    messageCmd = SendMessage <$> argument auto (metavar "SID" <> help "Session ID") <*> strOption (long "title" <> short 't') <*> strOption (long "message" <> short 'm')

main :: IO ()
main = execParser cmdParser >>= \case
    SessionsList -> do
      let curServer = wTS_CURRENT_SERVER_HANDLE
      sessions <- enumerateSessions curServer
      putStrLn $ printf "There are %d sessions." (length sessions)
      consoleSid <- getActiveConsoleSessionId
      putStrLn $ printf "Current console session: %s" (show consoleSid)
      mapM_
        (\s -> do
          let sid = wsiSessionId s
          isConsole <- isConcoleSession sid
          state <- quetyConnectionState curServer sid
          protoType <- queryClientProtocolType curServer sid
          winStationName <- queryWinStationName curServer sid
          domain <- querySessionDomainName curServer sid
          userName <- querySessionUserName curServer sid
          clientName <- queryClientName curServer sid
          clientBuildNum <- queryClientBuildNumber curServer sid
          (WTS_CLIENT_DISPLAY hRes vRes colorDepth) <- queryClientDisplay curServer sid
          putStrLn $ printf "\n%sSession SID: %u state: %s Protocol: %s"
            (if isConsole then "Console " else "") sid (show state) (show protoType)
          putStrLn $ printf "\tWinStationName: %s\n\tDomain: %s\n\tUserName: %s"
            winStationName domain userName
          putStrLn $ printf "Client:\n\tClientName: %s\n\tDisplay: %i x %i\n\tColorDepth: %i-bit\n\tBuildNumber: %d"
            clientName hRes vRes colorDepth clientBuildNum
          )
        sessions
    Disconnect sid ->
      disconnectSession wTS_CURRENT_SERVER_HANDLE sid False
    Logoff sid ->
      logoffSession wTS_CURRENT_SERVER_HANDLE sid False
    SendMessage sid title message ->
      void $ sendMessage wTS_CURRENT_SERVER_HANDLE sid (T.pack title) (T.pack message) MB_OK 0 False
  where
    cmdParser = info commandParse (fullDesc <> progDesc "Win32-wts test tool")
