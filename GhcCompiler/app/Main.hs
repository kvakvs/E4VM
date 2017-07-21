module Main where

import           AsmMod
import           BeamSParser
import           BytecodeMod
import           Term
import           TransformAsm
import           TransformS

import           System.Environment
import           System.IO
import           System.Log.Handler.Syslog
import           System.Log.Logger

transpile :: String -> String -> BytecodeMod.Module
transpile fn input = result
  where
    s1 = stageParseBeamS input
    s2 = stageBeamSToUasm s1
    result = stageCompileAsm s2

-- Given beam .S file contents (string) produce a Term tree structure with
-- parsed erlang values
stageParseBeamS :: String -> Term
stageParseBeamS = BeamSParser.parseS

-- Given Term tree produce microassembly data structure
stageBeamSToUasm :: Term -> AsmMod.Module
stageBeamSToUasm = TransformS.transform

stageCompileAsm :: AsmMod.Module -> BytecodeMod.Module
stageCompileAsm = TransformAsm.transform

initLogging :: IO ()
initLogging = do
  s <- openlog "SyslogStuff" [PID] USER DEBUG
  updateGlobalLogger rootLoggerName (addHandler s)
  updateGlobalLogger "uerlc" (setLevel DEBUG)

main :: IO ()
main = do
  initLogging
  [fileName] <- getArgs
  fh <- openFile fileName ReadMode
  contents <- hGetContents fh
  print $ transpile fileName contents
