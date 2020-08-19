{-# LANGUAGE LambdaCase #-}
module Main (main) where

import Data.Monoid ((<>))
import Options.Applicative
import Text.Printf

import System.Win32.WTS
import System.Win32.WTS.SessionInformation

data Command
  = SessionsList
  deriving (Eq, Show)

commandParse :: Parser Command
commandParse = subparser (
    command "sessions-list" (info (pure SessionsList) (progDesc "Show all sessions at the current machine"))
  )

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
  where
    cmdParser = info commandParse (fullDesc <> progDesc "Win32-wts test tool")
