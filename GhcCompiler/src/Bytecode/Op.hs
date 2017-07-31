module Bytecode.Op
  ( Opcode(..)
  , Instruction(..)
  ) where

import qualified Bytecode.Bits as BB
import           Uerlc

--import           Data.Ix
import qualified Data.List     as L
import           Data.Maybe    (fromJust)
import           Data.Tuple

--import           Data.Word
data Opcode
  = Alloc
  | CallBif
  | CallBifGc
  | CallGc
  | Call
  | CallTail
  | CallTailDealloc
  | Decons
  | Error
  | Jump
  | Move
  | Ret0
  | RetN
  | SelectVal
  | SelectTupleArity
  | Test
  | TestHeap
  | TupleGetEl
  | TupleNew
  | TuplePut
  | TupleSetEl
  deriving (Eq, Ord)

instance Show Opcode where
  show Alloc            = "+alloc"
  show CallBif          = "+bif"
  show CallBifGc        = "+bif/gc"
  show CallGc           = "+call/gc"
  show Call             = "+call"
  show CallTail         = "+call/tail"
  show CallTailDealloc  = "+call/tail/dealloc"
  show Decons           = "+decons"
  show Error            = "+err"
  show Jump             = "+jmp"
  show Move             = "+move"
  show Ret0             = "+ret0"
  show RetN             = "+retn"
  show SelectTupleArity = "+selectarity"
  show SelectVal        = "+selectval"
  show Test             = "+test"
  show TestHeap         = "+testheap"
  show TupleGetEl       = "+t_get"
  show TupleNew         = "+t_new"
  show TuplePut         = "+t_put"
  show TupleSetEl       = "+t_set"

bcOpEnumTable :: [(Opcode, Int)]
bcOpEnumTable =
  [ (Error, 0)
  , (Test, 1)
  , (Alloc, 2)
  , (Move, 3)
  , (Call, 4)
  , (CallGc, 5)
  , (CallTail, 6)
  , (CallTailDealloc, 7)
  , (Ret0, 8)
  , (RetN, 9)
  , (TupleNew, 10)
  , (TuplePut, 11)
  , (TupleGetEl, 12)
  , (TupleSetEl, 13)
  , (TestHeap, 14)
  , (CallBif, 15)
  , (CallBifGc, 16)
  , (Decons, 17)
  , (SelectVal, 18)
  , (SelectTupleArity, 19)
  , (Jump, 20)
  ]

instance Enum Opcode where
  fromEnum = fromJust . flip L.lookup bcOpEnumTable
  toEnum = fromJust . flip lookup (map swap bcOpEnumTable)

-- A combination of {Opcode and [Bit Encoded Args]}
data Instruction =
  Instruction Opcode
              BB.BitsList

instance Show Instruction where
  show (Instruction op bits) = show (fromEnum op) ++ " " ++ show bits ++ comment
    where
      comment = ansiCyan ++ " ; " ++ show op ++ ansiReset
