module Main where

import BeamSParser
import System.IO
import System.Environment
import System.Log.Logger
import System.Log.Handler.Syslog
import System.Log.Handler.Simple
import System.Log.Handler (setFormatter)
import System.Log.Formatter


transpile :: String -> String -> Expr
transpile fileName contents =
  runStage "parse beam assembly" (stageParseBeamS contents)


runStage :: String -> Either String out -> out
runStage descr result =
  case result of
    Left e -> error $ descr ++ e;
    Right outV -> outV


stageParseBeamS :: String -> Either String Expr
stageParseBeamS contents = BeamSParser.parseS contents


initLogging = do
  s <- openlog "SyslogStuff" [PID] USER DEBUG
  updateGlobalLogger rootLoggerName (addHandler s)
  updateGlobalLogger "uerlc" (setLevel DEBUG)


main = do
  initLogging
  [fileName]  <- getArgs
  fh <- openFile fileName ReadMode
  contents <- hGetContents fh
  print $ transpile fileName contents
