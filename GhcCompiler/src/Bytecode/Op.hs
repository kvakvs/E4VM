module Bytecode.Op where

import Bytecode.Bits

import qualified Data.List  as L
import           Data.Maybe (fromJust)
import           Data.Tuple
import           Data.Word

data BcOpcode
  = BcOpError
  | BcOpTest
  deriving (Show, Eq)

bcOpEnumTable :: [(BcOpcode, Int)]
bcOpEnumTable = [(BcOpError, 0), (BcOpTest, 1)]

instance Enum BcOpcode where
  fromEnum = fromJust . flip L.lookup bcOpEnumTable
  toEnum = fromJust . flip lookup (map swap bcOpEnumTable)

data BcOp =
  BcOp BcOpcode
       BitSequence
  deriving (Show)
