module Main where

import qualified Asm.Mod                   as AM
import qualified BeamSParser               as P0
import qualified Bytecode.Encode.Huffman   as Huff
import qualified Bytecode.Mod              as BM
import qualified Pass.PassAsm              as P2
import qualified Pass.PassBeamS            as P1
import qualified Term                      as T

import           Data.Word                 (Word8)
import           System.Environment
import           System.IO
import           System.Log.Handler.Syslog
import           System.Log.Logger

transpile :: String -> String -> IO BM.Module
transpile _fileName input = do
  let s1 = stageParseBeamS input
  print s1
  let s2 = stageBeamSToUasm s1
  print s2
  let result = stageCompileAsm s2
  print result
  return result

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

huffInput =
  [0, 0, 1, 1, 1, 1, 2, 2, 3, 4, 5, 5, 5, 5, 5, 5, 5, 6, 6, 7] :: [Word8]

main :: IO ()
main = do
  initLogging
  [fileName] <- getArgs
  fh <- openFile fileName ReadMode
  contents <- hGetContents fh
  result <- transpile fileName contents
  --
  let encoder = Huff.makeEncoderFromImport huffInput
  print encoder
  putStr "encodeSome [5]="
  print $ Huff.encodeSome encoder [5]
  putStr "encodeSome [1]="
  print $ Huff.encodeSome encoder [1]
  putStr "encodeSome [7]="
  print $ Huff.encodeSome encoder [7]
