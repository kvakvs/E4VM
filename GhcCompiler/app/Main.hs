module Main where

import BeamSParser
import BeamSTypes
import PassFromS
import UModule

import System.IO
import System.Environment
import System.Log.Logger
import System.Log.Handler.Syslog


transpile :: String -> String -> Module
transpile fileName contents =
  let stage1 = runStage fileName "parse BEAM assembly" (stageParseBeamS contents)
  in runStage fileName "convert to microAssembly" (stageBeamSToUasm stage1)


-- Given SExpr tree produce microassembly data structure
stageBeamSToUasm :: SExpr -> Either String Module
stageBeamSToUasm = PassFromS.transform


-- Given beam .S file contents (string) produce a SExpr tree structure with
-- parsed erlang values
stageParseBeamS :: String -> Either String SExpr
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
