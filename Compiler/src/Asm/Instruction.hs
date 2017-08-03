module Asm.Instruction
  ( SelectSubj(..)
  , Instruction(..)
  , CallType(..)
  , BuiltinError(..)
  , JumpTab(..)
  ) where

import           Asm.Binary
import           Asm.Locations
import qualified Term          as T

data SelectSubj
  = SValue
  | STupleArity

instance Show SelectSubj where
  show SValue      = "-value"
  show STupleArity = "-tuple-arity"

-- flags and options for a call
data CallType
  = NormalCall
  | GcEnabledCall Int
  | TailCall
  | TailCallDealloc Int

instance Show CallType where
  show NormalCall          = "-normal"
  show TailCall            = "-tail"
  show (GcEnabledCall n)   = "-gc:" ++ show n
  show (TailCallDealloc n) = "-tail -dealloc:" ++ show n

-- bad stuff (shorthand opcodes which cause exceptions)
data BuiltinError
  = EBadArg
  | EBadMatch ReadLoc
  | ECaseClause
  | EFunClause
  | EIfClause

instance Show BuiltinError where
  show EFunClause    = "function_clause"
  show EIfClause     = "if_clause"
  show ECaseClause   = "case_clause"
  show EBadArg       = "badarg"
  show (EBadMatch s) = "badmatch " ++ show s

type JumpTab = [(T.Term, LabelLoc)]

data Instruction
  = AAlloc Int
           Int
  -- convert matchstate in rxy to a (sub)binary
  | ABsContextToBin ReadLoc
  -- alloc arg0 words, arg1 live for gc, store result into writelocs
  | ABsInit Int
            Int
            WriteLoc
            LabelLoc
  -- store integer with unit/width/flags into writeloc
  | ABsPutInteger ReadLoc
                  BinaryFlags
                  WriteLoc
  | ABsRestore ReadLoc
               Int
  -- save match offset into save array[i]
  | ABsSave ReadLoc
            Int
  | ACallBif String
             LabelLoc
             [ReadLoc]
             CallType
             WriteLoc
  | ACall Int
          CodeLoc
          CallType
  | ACallFun Int
  | AComment String
  | ACons ReadLoc
          ReadLoc
          WriteLoc
  | ADealloc Int
  | ADecons ReadLoc
            WriteLoc
            WriteLoc
  | AError BuiltinError
  | AJump LabelLoc
  | ALabel LabelLoc
  | ALine Int
  | AMakeFun LabelLoc
             Int
  | AMove ReadLoc
          WriteLoc
  | ARet Int
  -- From a jumptable select a pair {value,label} and jump there, else jump to
  -- the onfail location
  | ASelect SelectSubj
            ReadLoc
            LabelLoc
            JumpTab
  | ASetNil WriteLoc
  | ATest String
          LabelLoc
          [ReadLoc]
          (Maybe Int)
          WriteLoc
  | ATestHeap Int
              Int
  | ATrim Int
  | ATupleGetEl ReadLoc
                ReadLoc
                WriteLoc
  | ATupleSetEl ReadLoc
                ReadLoc
                WriteLoc
  | ATupleNew Int
              WriteLoc
  | ATuplePut ReadLoc

show1 :: String -> [String] -> String
show1 s args = s ++ " " ++ unwords args

instance Show Instruction where
  show (AAlloc a b) = show1 "alloc" [show a, show b]
  show (ABsContextToBin a) = show1 "bsctx2bin" [show a]
  show (ABsInit a b c d) = show1 "bsinit" [show a, show b, show c, show d]
  show (ABsPutInteger a b c) = show1 "bsputi" [show a, show b, show c]
  show (ABsRestore a b) = show1 "bsrest" [show a, show b]
  show (ABsSave a b) = show1 "bssave" [show a, show b]
  show (ACallBif a b c d e) =
    show1 "callbif" [show a, show b, show c, show d, show e]
  show (ACall a b c) = show1 "call" [show a, show b, show c]
  show (ACallFun a) = show1 "callf" [show a]
  show (AComment a) = show1 ";" [show a]
  show (ACons a b c) = show1 "cons" [show a, show b, show c]
  show (ADealloc a) = show1 "dealloc" [show a]
  show (ADecons a b c) = show1 "decons" [show a, show b, show c]
  show (AError a) = show1 "err" [show a]
  show (AJump a) = show1 "jmp" [show a]
  show (ALabel a) = show1 "; label" [show a]
  show (ALine a) = show1 "; line" [show a]
  show (AMakeFun a b) = show1 "mkfun" [show a, show b]
  show (AMove a b) = show1 "move" [show a, show b]
  show (ARet a) = show1 "ret" [show a]
  show (ASelect a b c d) = show1 "select" [show a, show b, show c, show d]
  show (ASetNil a) = show1 "nil" [show a]
  show (ATest a b c d e) = show1 "test" [show a, show b, show c, show d, show e]
  show (ATestHeap a b) = show1 "testheap" [show a, show b]
  show (ATrim a) = show1 "trim" [show a]
  show (ATupleGetEl a b c) = show1 "getel" [show a, show b, show c]
  show (ATupleSetEl a b c) = show1 "setel" [show a, show b, show c]
  show (ATupleNew a b) = show1 "mktuple" [show a, show b]
  show (ATuplePut a) = show1 "put" [show a]
