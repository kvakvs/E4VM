module Main where

import qualified Asm.Mod                   as AM
import qualified BeamSParser               as P0
import qualified Bytecode.Mod              as BM
import qualified Pass.PassAsm              as P2
import qualified Pass.PassBeamS            as P1
import qualified Term                      as T

import           System.Environment
import           System.IO
import           System.Log.Handler.Syslog
import           System.Log.Logger

transpile :: String -> String -> BM.Module
transpile fn input = result
  where
    s1 = stageParseBeamS input
    s2 = stageBeamSToUasm s1
    result = stageCompileAsm s2

-- Given beam .S file contents (string) produce a Term tree structure with
-- parsed erlang values
stageParseBeamS :: String -> T.Term
stageParseBeamS = P0.transform

-- Given Term tree produce microassembly data structure
stageBeamSToUasm :: T.Term -> AM.Module
stageBeamSToUasm = P1.transform

stageCompileAsm :: AM.Module -> BM.Module
stageCompileAsm = P2.transform

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
