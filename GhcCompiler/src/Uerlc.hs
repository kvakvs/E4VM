module Uerlc
  ( err
  , errM
  , CompileErrorOr
  , CompileException(..)
  ) where

import           Control.Exception
import           Data.Typeable
import           System.Console.ANSI

newtype CompileException =
  CompileException String
  deriving (Typeable)

instance Show CompileException where
  show (CompileException s) = ansiRed ++ s ++ ansiColorOff

instance Exception CompileException

-- To combine with success value and create a monadic error type
type CompileErrorOr = Either CompileException

err :: String -> a
err s = throw $ CompileException s

-- Left value for use with monadic CompileErrorOr x
errM :: String -> Either CompileException b
errM s = Left $ CompileException s

-- Helper to paint the text red
ansiRed = setSGRCode [SetColor Foreground Vivid Red]

-- Helper to uncolor the text
ansiColorOff = setSGRCode [Reset]
