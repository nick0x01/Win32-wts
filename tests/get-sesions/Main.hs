module Main (main) where

import System.Win32.WTS
import Text.Printf

main :: IO ()
main = do
  let curServer = wTS_CURRENT_SERVER_HANDLE
  sessions <- enumerateSessions curServer
  putStrLn $ printf "%d sessions is active." (length sessions)
  mapM_
    (\s -> do
      let sid = wsiSessionId s
      protoType <- querySessionProtocol curServer sid
      putStrLn $ printf "sessionId: %u name: %s state: %s protoType: %s" sid (wsiWinStationName s) (show $ wsiState s) (show protoType)
      )
    sessions
