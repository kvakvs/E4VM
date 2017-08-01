module Asm where

import qualified Term as T

data SelectSubj
  = SValue
  | STupleArity

instance Show SelectSubj where
  show SValue      = "-value"
  show STupleArity = "-tuple-arity"

data LabelLoc
  = LabelLoc Int
  | NoLabel

instance Show LabelLoc where
  show (LabelLoc i) = "-label:" ++ show i
  show NoLabel      = "-no-label"

data CodeLoc
  = CLabel LabelLoc
  | CExtFunc String
             String
             Int

instance Show CodeLoc where
  show (CLabel ulbl)    = show ulbl
  show (CExtFunc m f a) = "@" ++ m ++ ":" ++ f ++ "/" ++ show a

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

data BinUnitWidth =
  BinUnitWidth Int
               Int

-- options for binary value: (unitsize, signed, bigendian)
data BinaryFlags = BinaryFlags
  { bfUnitSize :: Int
  , bfSigned :: Bool
  , bfBig :: Bool
  }

instance Show BinaryFlags where
  show (BinaryFlags u sig big) =
    "unit:" ++ show u ++ "/" ++ sigStr ++ "/" ++ bigStr
    where
      sigStr =
        if sig
          then "signed"
          else "unsigned"
      bigStr =
        if big
          then "big"
          else "little"

--instance Show BinaryFlags where
--  show bf = "u=" ++ show (bfUnitSize bf) ++ ";" ++ signed ++ ";" ++ big
--    where signed = if bfSigned bf then "signed" else "unsigned"
--          big = if bfBig bf then "big" else "little"
--
-- sources of the data (registers, literals, immediates...)
data ReadLoc
  = RRegX Int
  | RRegY Int
  | RAtom String
  | RInt Integer
  | RLit T.Term
  | RNil
  | RBinaryFlags BinaryFlags

rarrow :: String
rarrow = "➚"

warrow :: String
warrow = "➘"

instance Show ReadLoc where
  show (RRegX i)         = "x" ++ show i ++ rarrow
  show (RRegY i)         = "y" ++ show i ++ rarrow
  show (RAtom a)         = show a
  show (RInt i)          = show i
  show (RLit lit)        = "lit:" ++ show lit
  show RNil              = "[]"
  show (RBinaryFlags bf) = "-bin-flags:" ++ show bf

-- where you can possibly store the value
data WriteLoc
  = WRegX Int
  | WRegY Int
  | WIgnore

instance Show WriteLoc where
  show (WRegX i) = warrow ++ "x" ++ show i
  show (WRegY i) = warrow ++ "y" ++ show i
  show WIgnore   = warrow ++ "drop"

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

jump :: LabelLoc -> Instruction
jump = AJump

label :: Int -> Instruction
label i = ALabel (LabelLoc i)

comment :: Show a => a -> Instruction
comment x = AComment $ show x

ret :: Int -> Instruction
ret = ARet

move :: ReadLoc -> WriteLoc -> Instruction
move = AMove

funcClause :: Instruction
funcClause = AError EFunClause

caseClause :: Instruction
caseClause = AError ECaseClause

ifClause :: Instruction
ifClause = AError EIfClause

badarg :: Instruction
badarg = AError EBadArg

badmatch :: ReadLoc -> Instruction
badmatch r = AError (EBadMatch r)

tupleNew :: Int -> WriteLoc -> Instruction
tupleNew = ATupleNew

tuplePut :: ReadLoc -> Instruction
tuplePut = ATuplePut

tupleGetEl :: ReadLoc -> ReadLoc -> WriteLoc -> Instruction
tupleGetEl = ATupleGetEl

tupleSetEl :: ReadLoc -> ReadLoc -> WriteLoc -> Instruction
tupleSetEl = ATupleSetEl

testHeap :: Int -> Int -> Instruction
testHeap = ATestHeap

allocate :: Int -> Int -> Instruction
allocate = AAlloc

deallocate :: Int -> Instruction
deallocate = ADealloc

test :: String -> LabelLoc -> [ReadLoc] -> Maybe Int -> WriteLoc -> Instruction
test = ATest

callLabel :: Int -> LabelLoc -> CallType -> Instruction
callLabel arity lbl = ACall arity (CLabel lbl)

callExt :: (String, String, Int) -> CallType -> Instruction
callExt (m, f, arity) = ACall arity (CExtFunc m f arity)

callBif ::
     String -> LabelLoc -> [ReadLoc] -> CallType -> WriteLoc -> Instruction
callBif = ACallBif

decons :: ReadLoc -> WriteLoc -> WriteLoc -> Instruction
decons = ADecons

select :: SelectSubj -> ReadLoc -> LabelLoc -> JumpTab -> Instruction
select = ASelect

cons :: ReadLoc -> ReadLoc -> WriteLoc -> Instruction
cons = ACons

callFun :: Int -> Instruction
callFun = ACallFun

setNil :: WriteLoc -> Instruction
setNil = ASetNil

trim :: Int -> Instruction
trim = ATrim

makeFun :: LabelLoc -> Int -> Instruction
makeFun = AMakeFun

bsContextToBin :: ReadLoc -> Instruction
bsContextToBin = ABsContextToBin

bsSave :: ReadLoc -> Int -> Instruction
bsSave = ABsSave

bsRestore :: ReadLoc -> Int -> Instruction
bsRestore = ABsRestore

bsInit :: Int -> Int -> WriteLoc -> LabelLoc -> Instruction
bsInit = ABsInit

bsPutInteger :: ReadLoc -> BinaryFlags -> WriteLoc -> Instruction
bsPutInteger = ABsPutInteger
