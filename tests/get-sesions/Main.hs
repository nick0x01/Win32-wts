module Main (main) where

import System.Win32.SystemServices.TerminalServices
import Text.Printf

main :: IO ()
main = do
  let curServer = wTS_CURRENT_SERVER_HANDLE
  sessions <- enumerateSessions curServer
  putStrLn $ printf "%d sessions is active." (length sessions)
  mapM_
    (\s -> do
      let sid = sessionId s
      protoType <- querySessionProtocol curServer sid
      putStrLn $ printf "  %d session have %s protocol type." sid $ show protoType
      )
    sessions
