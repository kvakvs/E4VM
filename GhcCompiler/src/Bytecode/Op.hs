module Bytecode.Op where

import           Bytecode.Bits
import           Uerlc

import qualified Data.List     as L
import           Data.Maybe    (fromJust)
import           Data.Tuple
import           Data.Word

data BcOpcode
  = BcOpError
  | BcOpTest
  | BcOpAlloc
  | BcOpTGetEl
  deriving (Eq)

instance Show BcOpcode where
  show BcOpError = "+err"
  show BcOpTest = "+test"
  show BcOpAlloc = "+alloc"
  show BcOpTGetEl = "+tgetel"

bcOpEnumTable :: [(BcOpcode, Int)]
bcOpEnumTable = [(BcOpError, 0), (BcOpTest, 1), (BcOpAlloc, 2)]

instance Enum BcOpcode where
  fromEnum = fromJust . flip L.lookup bcOpEnumTable
  toEnum = fromJust . flip lookup (map swap bcOpEnumTable)

data BcOp =
  BcOp BcOpcode
       BitStringList

instance Show BcOp where
  show (BcOp opcode bits) =
    show (fromEnum opcode) ++ " " ++ show bits ++ comment
    where
      comment = ansiCyan ++ " ; " ++ show opcode ++ ansiReset
