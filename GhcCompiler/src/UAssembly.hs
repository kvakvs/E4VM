module UAssembly where

import           BeamSTypes

newtype Label =
  MakeLabel Int
  deriving (Show)

data ReadLoc
  = RRegX Int
  | RRegY Int
  | RAtom SExpr
  | RInt Integer
  | RLit SExpr
  | RNil
  | ReadLocError String
  deriving (Show)

data WriteLoc
  = WRegX Int
  | WRegY Int
  | WriteLocError String
  deriving (Show)

data BuiltinError
  = EFunClause
  | EBadArg
  deriving (Show)

data UAsmOp
  = ALabel Label
  | ALine Int
  | ARet Int
  | AMove ReadLoc
          WriteLoc
  | AError BuiltinError
  | ATupleNew Int
              WriteLoc
  | ATuplePut ReadLoc
  | ATupleGetEl ReadLoc
                ReadLoc
                WriteLoc
  | AAlloc Int
           Int
  | ADealloc Int
  | ATest String
          Label
          [ReadLoc]
  | AComment String
  deriving (Show)

label :: Integral a => a -> UAsmOp
label i = ALabel (MakeLabel (fromIntegral i))

comment :: Show a => a -> UAsmOp
comment x = AComment $ show x

ret0 :: UAsmOp
ret0 = ARet 0

ret :: Integral a => a -> UAsmOp
ret n = ARet (fromIntegral n)

move :: ReadLoc -> WriteLoc -> UAsmOp
move = AMove

funcClause :: UAsmOp
funcClause = AError EFunClause

badarg :: UAsmOp
badarg = AError EBadArg

tupleNew :: Integral a => a -> WriteLoc -> UAsmOp
tupleNew sz = ATupleNew (fromIntegral sz)

tuplePut :: ReadLoc -> UAsmOp
tuplePut = ATuplePut

tupleGetEl :: ReadLoc -> ReadLoc -> WriteLoc -> UAsmOp
tupleGetEl = ATupleGetEl

allocate :: (Integral ts, Integral tl) => ts -> tl -> UAsmOp
allocate stkneed live = AAlloc (fromIntegral stkneed) (fromIntegral live)

deallocate :: Integral a => a -> UAsmOp
deallocate n = ADealloc (fromIntegral n)

test :: String -> Label -> [ReadLoc] -> UAsmOp
test = ATest
