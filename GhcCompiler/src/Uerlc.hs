module Uerlc
  ( err
  ) where

import           Control.Exception
import           Data.Typeable

newtype CompileException =
  CompileException String
  deriving (Show, Typeable)

instance Exception CompileException

err :: String -> a
err s = throw $ CompileException s
