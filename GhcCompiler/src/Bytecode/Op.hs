module Bytecode.Op where

import           Bytecode.Bits
import           Uerlc

import           Data.Ix

--import qualified Data.List     as L
--import           Data.Maybe    (fromJust)
--import           Data.Tuple
--import           Data.Word
data BcOpcode
  = BcOpFIRST__
  | BcOpError
  | BcOpTest
  | BcOpAlloc
  | BcOpTGetEl
  | BcOpLAST__
  deriving (Eq, Ix, Ord)

instance Show BcOpcode where
  show BcOpFIRST__ = "?first" -- unused
  show BcOpLAST__  = "?last" -- unused
  show BcOpError   = "+err"
  show BcOpTest    = "+test"
  show BcOpAlloc   = "+alloc"
  show BcOpTGetEl  = "+tgetel"

--bcOpEnumTable :: [(BcOpcode, Int)]
--bcOpEnumTable = [(BcOpError, 0), (BcOpTest, 1), (BcOpAlloc, 2)]
opIndex :: BcOpcode -> Int
opIndex op = index (BcOpFIRST__, BcOpLAST__) op - 1

--instance Enum BcOpcode where
--  fromEnum = fromJust . flip L.lookup bcOpEnumTable
--  toEnum = fromJust . flip lookup (map swap bcOpEnumTable)
data BcOp =
  BcOp BcOpcode
       BitStringList

instance Show BcOp where
  show (BcOp opcode bits) = show (opIndex opcode) ++ " " ++ show bits ++ comment
    where
      comment = ansiCyan ++ " ; " ++ show opcode ++ ansiReset
