module Bytecode.Op where

import           Bytecode.Bits
import           Uerlc

import           Data.Ix

import qualified Data.List     as L
import           Data.Maybe    (fromJust)
import           Data.Tuple
import           Data.Word

data BcOpcode
  = BcOpError
  | BcOpTest
  | BcOpAlloc
  | BcOpTGetEl
  | BcOpMove
  | BcOpCallNormal
  | BcOpCallGc
  | BcOpCallTail
  | BcOpCallTailDealloc
  deriving (Eq, Ord)

instance Show BcOpcode where
  show BcOpError  = "+err"
  show BcOpTest   = "+test"
  show BcOpAlloc  = "+alloc"
  show BcOpTGetEl = "+tgetel"
  show BcOpMove   = "+move"
  show BcOpCallNormal = "+call"
  show BcOpCallGc= "+callgc"
  show BcOpCallTail = "+calltail"
  show BcOpCallTailDealloc  = "+calltaild"

bcOpEnumTable :: [(BcOpcode, Int)]
bcOpEnumTable =
  [ (BcOpError, 0)
  , (BcOpTest, 1)
  , (BcOpAlloc, 2)
  , (BcOpTGetEl, 3)
  , (BcOpMove, 4)
  , (BcOpCallNormal, 5)
  , (BcOpCallGc, 6)
  , (BcOpCallTail, 7)
  , (BcOpCallTailDealloc, 8)
  ]

instance Enum BcOpcode where
  fromEnum = fromJust . flip L.lookup bcOpEnumTable
  toEnum = fromJust . flip lookup (map swap bcOpEnumTable)

data BcOp =
  BcOp BcOpcode
       BitStringList

instance Show BcOp where
  show (BcOp op bits) = show (fromEnum op) ++ " " ++ show bits ++ comment
    where
      comment = ansiCyan ++ " ; " ++ show op ++ ansiReset
