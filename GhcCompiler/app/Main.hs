module Main where

import BeamSParser
import System.IO
import System.Environment

main = do
  [fileName]  <- getArgs
  fh <- openFile fileName ReadMode
  contents <- hGetContents fh
  case BeamSParser.parseS contents of
    Left e -> error $ show e;
    Right result -> print result
