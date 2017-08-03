module Main where

import qualified Asm.Mod                   as AM
import qualified BeamSParser               as P0
import qualified Bitcode.Encode.Huffman   as Huff
import qualified Bitcode.Mod              as BM
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
  let s1 = P0.transform input
  print s1
  let s2 = P1.transform s1
  print s2
  let bitcodeMod = P2.transform s2
  return bitcodeMod

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
  bitcodeMod <- transpile fileName contents
  --
  putStrLn "done"
  print bitcodeMod
  print $ BM.opStats bitcodeMod
