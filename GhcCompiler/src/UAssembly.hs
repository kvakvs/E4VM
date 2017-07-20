module UAssembly where

import           BeamSTypes

data ULbl
  = ULbl Int
  | UNoLabel

instance Show ULbl where
  show (ULbl i) = "@" ++ show i
  show UNoLabel = "@?"

data CodeLoc
  = CLabel ULbl
  | CExtFunc String
             String
             Int

instance Show CodeLoc where
  show (CLabel ulbl)    = show ulbl
  show (CExtFunc m f a) = "@" ++ m ++ ":" ++ f ++ "/" ++ show a

data UCallType
  = NormalCall
  | TailCallDealloc Int

instance Show UCallType where
  show NormalCall          = "-normal"
  show (TailCallDealloc n) = "-tail " ++ show n ++ ")"

data ReadLoc
  = RRegX Int
  | RRegY Int
  | RAtom SExpr
  | RInt Integer
  | RLit SExpr
  | RNil
  | ReadLocError String

instance Show ReadLoc where
  show (RRegX i)        = "←X" ++ show i
  show (RRegY i)        = "←Y" ++ show i
  show (RAtom a)        = show a
  show (RInt i)         = show i
  show (RLit lit)       = show lit
  show RNil             = "[]"
  show (ReadLocError s) = "error: " ++ s

data WriteLoc
  = WRegX Int
  | WRegY Int
  | WriteLocError String

instance Show WriteLoc where
  show (WRegX i)         = "→X" ++ show i
  show (WRegY i)         = "→Y" ++ show i
  show (WriteLocError s) = "error: " ++ s

data BuiltinError
  = EFunClause
  | EBadArg
  | EBadMatch ReadLoc

instance Show BuiltinError where
  show EFunClause    = "function_clause"
  show EBadArg       = "badarg"
  show (EBadMatch s) = "badmatch " ++ show s

data UAsmOp
  = AAlloc Int
           Int
  | ACallBif String
             ULbl
             [ReadLoc]
             UCallType
             WriteLoc
  | ACall Int
          CodeLoc
          UCallType
  | AComment String
  | ADealloc Int
  | ADecons ReadLoc
            WriteLoc
            WriteLoc
  | AError BuiltinError
  | ALabel ULbl
  | ALine Int
  | AMove ReadLoc
          WriteLoc
  | ARet Int
  | ASelect ReadLoc
            ULbl
            [(SExpr, ULbl)]
  | ATest String
          ULbl
          [ReadLoc]
  | ATestHeap Int
              Int
  | ATupleGetEl ReadLoc
                ReadLoc
                WriteLoc
  | ATupleNew Int
              WriteLoc
  | ATuplePut ReadLoc
  deriving (Show)

label :: Int -> UAsmOp
label i = ALabel (ULbl i)

comment :: Show a => a -> UAsmOp
comment x = AComment $ show x

ret :: Int -> UAsmOp
ret = ARet

move :: ReadLoc -> WriteLoc -> UAsmOp
move = AMove

funcClause :: UAsmOp
funcClause = AError EFunClause

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

test :: String -> ULbl -> [ReadLoc] -> UAsmOp
test = ATest

callLabel :: Int -> ULbl -> UCallType -> UAsmOp
callLabel arity lbl = ACall arity (CLabel lbl)

callExt :: (String, String, Int) -> UCallType -> UAsmOp
callExt (m, f, arity) = ACall arity (CExtFunc m f arity)

callBif :: String -> ULbl -> [ReadLoc] -> UCallType -> WriteLoc -> UAsmOp
callBif = ACallBif

decons :: ReadLoc -> WriteLoc -> WriteLoc -> UAsmOp
decons = ADecons

select :: ReadLoc -> ULbl -> [(SExpr, ULbl)] -> UAsmOp
select = ASelect
