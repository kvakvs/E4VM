{-# LANGUAGE UnicodeSyntax #-}

module UAssembly where

import           BeamSTypes

newtype Label =
  Label Int
  deriving (Show)

data ReadLoc
  = RRegX Integer
  | RRegY Integer
  | RAtom SExpr
  | RInt Integer
  | RLit SExpr
  | RNil
  | ReadLocError String
  deriving (Show)

data WriteLoc
  = WRegX Integer
  | WRegY Integer
  | WriteLocError String
  deriving (Show)

data BuiltinError
  = EFunClause
  | EBadArg
  deriving (Show)

data UAsmOp
  = ALabel Label
  | ALine Integer
  | ARet Int
  | AMove ReadLoc
          WriteLoc
  | AError BuiltinError
  | ATupleNew Integer
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
label i = ALabel (Label (fromIntegral i))

comment ∷ Show a ⇒ a → UAsmOp
comment x = AComment $ show x

ret0 :: UAsmOp
ret0 = ARet 0

ret :: Integral a => a -> UAsmOp
ret n = ARet (fromIntegral n)

move ∷ ReadLoc → WriteLoc → UAsmOp
move = AMove

funcClause ∷ UAsmOp
funcClause = AError EFunClause

badarg ∷ UAsmOp
badarg = AError EBadArg

tupleNew ∷ Integer → WriteLoc → UAsmOp
tupleNew = ATupleNew

tuplePut ∷ ReadLoc → UAsmOp
tuplePut = ATuplePut

tupleGetEl ∷ ReadLoc → ReadLoc → WriteLoc → UAsmOp
tupleGetEl = ATupleGetEl

allocate :: (Integral a, Integral a1) => a1 -> a -> UAsmOp
allocate stkneed live = AAlloc (fromIntegral stkneed) (fromIntegral live)

deallocate :: Integral a => a -> UAsmOp
deallocate n = ADealloc (fromIntegral n)

test ∷ String → Label → [ReadLoc] → UAsmOp
test = ATest
