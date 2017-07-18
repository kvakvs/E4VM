module Main where

import BeamSParser
import BeamSTypes
import MicroAsm
import UModule

import System.IO
import System.Environment
import System.Log.Logger
import System.Log.Handler.Syslog
import System.Log.Handler.Simple
import System.Log.Handler (setFormatter)
import System.Log.Formatter


transpile :: String -> String -> Module
transpile fileName contents =
  let stage1 = runStage fileName "parse BEAM assembly" (stageParseBeamS contents)
  in runStage fileName "convert to microAssembly" (stageGenMicroAsm stage1)


-- Given BeamSExpr tree produce microassembly data structure
stageGenMicroAsm = MicroAsm.transform


-- Given beam .S file contents (string) produce a BeamSExpr tree structure with
-- parsed erlang values
stageParseBeamS :: String -> Either String BeamSExpr
stageParseBeamS = BeamSParser.parseS


runStage :: String -> String -> Either String out -> out
runStage fileName descr result =
  case result of
    Left e -> error $ fileName ++ " -> " ++ descr ++ " stage ERROR: " ++ e;
    Right outV -> outV


initLogging :: IO ()
initLogging = do
  s <- openlog "SyslogStuff" [PID] USER DEBUG
  updateGlobalLogger rootLoggerName (addHandler s)
  updateGlobalLogger "uerlc" (setLevel DEBUG)


main :: IO ()
main = do
  initLogging
  [fileName]  <- getArgs
  fh <- openFile fileName ReadMode
  contents <- hGetContents fh
  print $ transpile fileName contents
