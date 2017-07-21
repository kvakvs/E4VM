module UAssembly where

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

data ReadLoc
  = RRegX Int
  | RRegY Int
  | RAtom SExpr
  | RInt Integer
  | RLit SExpr
  | RNil
  | ReadLocError String

instance Show ReadLoc where
  show (RRegX i)        = "X" ++ show i ++ "➔"
  show (RRegY i)        = "Y" ++ show i ++ "➔"
  show (RAtom a)        = show a
  show (RInt i)         = show i
  show (RLit lit)       = "lit:" ++ show lit
  show RNil             = "[]"
  show (ReadLocError s) = "ReadLocError(" ++ s ++ ")"

data WriteLoc
  = WRegX Int
  | WRegY Int
  | WIgnore
  | WriteLocError String

instance Show WriteLoc where
  show (WRegX i)         = "➔X" ++ show i
  show (WRegY i)         = "➔Y" ++ show i
  show WIgnore           = "➔ignore"
  show (WriteLocError s) = "WriteLocError(" ++ s ++ ")"

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

data UAsmOp
  = AAlloc Int
           Int
  | ABsContextToBin ReadLoc -- convert matchstate in rxy to a (sub)binary
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
  | ATupleNew Int
              WriteLoc
  | ATuplePut ReadLoc
  deriving (Show)

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
