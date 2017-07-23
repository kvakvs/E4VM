module Uerlc
  ( err
  , errM
  , CompileErrorOr
  , CompileException(..)
  ) where

import           Control.Exception
import           Data.Typeable

newtype CompileException =
  CompileException String
  deriving (Typeable)

instance Show CompileException where
  show (CompileException s) = s

instance Exception CompileException

-- To combine with success value and create a monadic error type
type CompileErrorOr = Either CompileException

err :: String -> a
err s = throw $ CompileException s

errM :: String -> Either CompileException b
errM s = Left $ CompileException s
