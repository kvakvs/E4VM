module Bytecode.Op
  ( BcOpcode(..)
  , BcOp(..)
  ) where

import           Bytecode.Bits
import           Uerlc

--import           Data.Ix
import qualified Data.List     as L
import           Data.Maybe    (fromJust)
import           Data.Tuple

--import           Data.Word
data BcOpcode
  = BcOpAlloc
  | BcOpCallBif
  | BcOpCallBifGc
  | BcOpCallGc
  | BcOpCallNormal
  | BcOpCallTail
  | BcOpCallTailDealloc
  | BcOpError
  | BcOpMove
  | BcOpRet0
  | BcOpRetN
  | BcOpTest
  | BcOpTestHeap
  | BcOpTupleGetEl
  | BcOpTupleNew
  | BcOpTuplePut
  | BcOpTupleSetEl
  deriving (Eq, Ord)

instance Show BcOpcode where
  show BcOpAlloc           = "+alloc"
  show BcOpCallGc          = "+call+gc"
  show BcOpCallNormal      = "+call"
  show BcOpCallTail        = "+call+t"
  show BcOpCallTailDealloc = "+call+td"
  show BcOpError           = "+err"
  show BcOpMove            = "+move"
  show BcOpTest            = "+test"
  show BcOpTestHeap        = "+testheap"
  show BcOpTupleGetEl      = "+t_get"
  show BcOpTupleNew        = "+t_new"
  show BcOpTuplePut        = "+t_put"
  show BcOpTupleSetEl      = "+t_set"
  show BcOpRet0            = "+ret0"
  show BcOpRetN            = "+retn"
  show BcOpCallBif         = "+bif"
  show BcOpCallBifGc       = "+bif+gc"

bcOpEnumTable :: [(BcOpcode, Int)]
bcOpEnumTable =
  [ (BcOpError, 0)
  , (BcOpTest, 1)
  , (BcOpAlloc, 2)
  , (BcOpMove, 3)
  , (BcOpCallNormal, 4)
  , (BcOpCallGc, 5)
  , (BcOpCallTail, 6)
  , (BcOpCallTailDealloc, 7)
  , (BcOpRet0, 8)
  , (BcOpRetN, 9)
  , (BcOpTupleNew, 10)
  , (BcOpTuplePut, 11)
  , (BcOpTupleGetEl, 12)
  , (BcOpTupleSetEl, 13)
  , (BcOpTestHeap, 14)
  , (BcOpCallBif, 15)
  , (BcOpCallBifGc, 16)
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
