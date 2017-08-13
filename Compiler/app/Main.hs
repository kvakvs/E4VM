module Main where

import qualified Asm.Mod                   as AM
import qualified BeamSParser               as P0
import qualified Bitcode.Encode.Huffman   as Huff
import qualified Bitcode.Mod              as BM
import qualified Pass.PassAsm              as P2
import qualified Pass.PassBeamS            as P1
import qualified Pass.PassBitcode as P3
import qualified Term                      as T

import           Data.Word                 (Word8)
import           System.Environment
import           System.IO
import           System.Log.Handler.Syslog
import           System.Log.Logger

-- Run chain of compile passes and print some intermediate results
transpile :: String -> String -> IO BM.Module
transpile _fileName input = do
  let s1 = P0.transform input -- pass 1 parse S file
  --
  let s2 = P1.transform s1 -- pass 2 compile S to microErlang Assembly
  print s2
  --
  let s3 = P2.transform s2 -- pass 3 encode args as bits and assign opcodes
  print s3
  --
  let bitcode = P3.transform s3 -- pass 4 produce bitstream, compress opcodes
  return bitcode

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
  bm0 <- transpile fileName contents
  --
  print bm0
  print $ BM.opStats bm0
  --
  putStrLn "done"
