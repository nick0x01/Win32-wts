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
      mapM_
        (\s -> do
          let sid = wsiSessionId s
          user <- querySessionUserName curServer sid
          domain <- querySessionDomainName curServer sid
          winStation <- queryWinStationName curServer sid
          protoType <- queryClientProtocolType curServer sid
          putStrLn $
            printf "sessionId: %u name: %s state: %s username: %s domain: %s winStation: %s protocolType: %s"
              sid (wsiWinStationName s) (show $ wsiState s) user domain winStation (show protoType)
          )
        sessions
  where
    cmdParser = info commandParse (fullDesc <> progDesc "Win32-wts test tool")
