module Asm where

import           BeamSTypes

data USelectSubject
  = SelectVal
  | SelectTupleArity
  deriving (Show)

data LabelLoc
  = LabelLoc Int
  | UNoLabel

instance Show LabelLoc where
  show (LabelLoc i) = "@" ++ show i
  show UNoLabel     = "@?"

data CodeLoc
  = CLabel LabelLoc
  | CExtFunc String
             String
             Int

instance Show CodeLoc where
  show (CLabel ulbl)    = show ulbl
  show (CExtFunc m f a) = "@" ++ m ++ ":" ++ f ++ "/" ++ show a

-- flags and options for a call
data UCallType
  = NormalCall
  | GcEnabledCall Int
  | TailCall
  | TailCallDealloc Int

instance Show UCallType where
  show NormalCall          = "-normal"
  show TailCall            = "-tail"
  show (GcEnabledCall n)   = "-gc:" ++ show n
  show (TailCallDealloc n) = "-tail -dealloc:" ++ show n

-- sources of the data (registers, literals, immediates...)
data ReadLoc
  = RRegX Int
  | RRegY Int
  | RAtom SExpr
  | RInt Integer
  | RLit SExpr
  | RNil
  | ReadLocError String

rarrow :: String
rarrow = "➚"
warrow :: String
warrow = "➘"

instance Show ReadLoc where
  show (RRegX i)        = "x" ++ show i ++ rarrow
  show (RRegY i)        = "y" ++ show i ++ rarrow
  show (RAtom a)        = show a
  show (RInt i)         = show i
  show (RLit lit)       = "lit:" ++ show lit
  show RNil             = "[]"
  show (ReadLocError s) = "ReadLocError(" ++ s ++ ")"

-- where you can possibly store the value
data WriteLoc
  = WRegX Int
  | WRegY Int
  | WIgnore
  | WriteLocError String

instance Show WriteLoc where
  show (WRegX i)         = warrow ++ "x" ++ show i
  show (WRegY i)         = warrow ++ "y" ++ show i
  show WIgnore           = warrow ++ "drop"
  show (WriteLocError s) = "WriteLocError(" ++ s ++ ")"

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

data BinUnitWidth =
  BinUnitWidth Int
               Int

-- options for binary value: (unitsize, signed, bigendian)
data BinaryFlags =
  BinaryFlags Int
              Bool
              Bool

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

data UAsmOp
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
             UCallType
             WriteLoc
  | ACall Int
          CodeLoc
          UCallType
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
  | ASelect USelectSubject
            ReadLoc
            LabelLoc
            [(SExpr, LabelLoc)]
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

show1 :: [Char] -> [String] -> [Char]
show1 s args = s ++ " " ++ unwords args

instance Show UAsmOp where
  show (AAlloc a b) = show1 "alloc" [show a, show b]
  show (ABsContextToBin a) = show1 "bsctx2bin" [show a]
  show (ABsInit a b c d) = show1 "bsinit" [show a, show b, show c, show d]
  show (ABsPutInteger a b c) = show1 "bsputi" [show a, show b, show c]
  show (ABsRestore a b) = show1 "bsrest" [show a, show b]
  show (ABsSave a b) = show1 "bssave" [show a, show b]
  show (ACallBif a b c d e) = show1 "callbif" [show a, show b, show c, show d, show e]
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

jump :: LabelLoc -> UAsmOp
jump = AJump

label :: Int -> UAsmOp
label i = ALabel (LabelLoc i)

comment :: Show a => a -> UAsmOp
comment x = AComment $ show x

ret :: Int -> UAsmOp
ret = ARet

move :: ReadLoc -> WriteLoc -> UAsmOp
move = AMove

funcClause :: UAsmOp
funcClause = AError EFunClause

caseClause :: UAsmOp
caseClause = AError ECaseClause

ifClause :: UAsmOp
ifClause = AError EIfClause

badarg :: UAsmOp
badarg = AError EBadArg

badmatch :: ReadLoc -> UAsmOp
badmatch r = AError (EBadMatch r)

tupleNew :: Int -> WriteLoc -> UAsmOp
tupleNew = ATupleNew

tuplePut :: ReadLoc -> UAsmOp
tuplePut = ATuplePut

tupleGetEl :: ReadLoc -> ReadLoc -> WriteLoc -> UAsmOp
tupleGetEl = ATupleGetEl

tupleSetEl :: ReadLoc -> ReadLoc -> WriteLoc -> UAsmOp
tupleSetEl = ATupleSetEl

testHeap :: Int -> Int -> UAsmOp
testHeap = ATestHeap

allocate :: Int -> Int -> UAsmOp
allocate = AAlloc

deallocate :: Int -> UAsmOp
deallocate = ADealloc

test :: String -> LabelLoc -> [ReadLoc] -> Maybe Int -> WriteLoc -> UAsmOp
test = ATest

callLabel :: Int -> LabelLoc -> UCallType -> UAsmOp
callLabel arity lbl = ACall arity (CLabel lbl)

callExt :: (String, String, Int) -> UCallType -> UAsmOp
callExt (m, f, arity) = ACall arity (CExtFunc m f arity)

callBif :: String -> LabelLoc -> [ReadLoc] -> UCallType -> WriteLoc -> UAsmOp
callBif = ACallBif

decons :: ReadLoc -> WriteLoc -> WriteLoc -> UAsmOp
decons = ADecons

select :: USelectSubject -> ReadLoc -> LabelLoc -> [(SExpr, LabelLoc)] -> UAsmOp
select = ASelect

cons :: ReadLoc -> ReadLoc -> WriteLoc -> UAsmOp
cons = ACons

callFun :: Int -> UAsmOp
callFun = ACallFun

setNil :: WriteLoc -> UAsmOp
setNil = ASetNil

trim :: Int -> UAsmOp
trim = ATrim

makeFun :: LabelLoc -> Int -> UAsmOp
makeFun = AMakeFun

bsContextToBin :: ReadLoc -> UAsmOp
bsContextToBin = ABsContextToBin

bsSave :: ReadLoc -> Int -> UAsmOp
bsSave = ABsSave

bsRestore :: ReadLoc -> Int -> UAsmOp
bsRestore = ABsRestore

bsInit :: Int -> Int -> WriteLoc -> LabelLoc -> UAsmOp
bsInit = ABsInit

bsPutInteger :: ReadLoc -> BinaryFlags -> WriteLoc -> UAsmOp
bsPutInteger = ABsPutInteger
