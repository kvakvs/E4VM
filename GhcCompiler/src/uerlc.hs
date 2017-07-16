import Tokenizer
import System.IO
import System.Environment
-- import Control.Monad
import Text.ParserCombinators.Parsec

main = do
  [fileName]  <- getArgs
  fh <- openFile fileName ReadMode
  contents <- hGetContents fh
  case parse Tokenizer.beamSParser "" contents of
    Left e -> error $ show e;
    Right result -> print result
