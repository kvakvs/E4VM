module Main where

import BeamSParser
import System.IO
import System.Environment
import System.Log.Logger
import System.Log.Handler.Syslog
import System.Log.Handler.Simple
import System.Log.Handler (setFormatter)
import System.Log.Formatter


transpile :: String -> String -> IO ()
transpile fileName contents =
  let out1 = stageParseBeamS contents
  in putStrLn "done"


stageParseBeamS :: String -> Expr
stageParseBeamS contents =
  case BeamSParser.parseS contents of
    Left e ->
      let io2 = errorM "uerlc" $ show e
      in ErlList [];
    Right result ->
      let io1 = infoM "uerlc" (show result)
      in result


initLogging = do
  s <- openlog "SyslogStuff" [PID] USER DEBUG
  updateGlobalLogger rootLoggerName (addHandler s)
  updateGlobalLogger "uerlc" (setLevel DEBUG)


main = do
  initLogging
  [fileName]  <- getArgs
  fh <- openFile fileName ReadMode
  contents <- hGetContents fh
  transpile fileName contents
