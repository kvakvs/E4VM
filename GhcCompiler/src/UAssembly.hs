{-# LANGUAGE UnicodeSyntax #-}

module UAssembly where

import           BeamSTypes

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
  = ALabel Integer
  | ALine Integer
  | ARet Integer
  | AMove ReadLoc
          WriteLoc
  | AError BuiltinError
  | ATupleNew Integer
              WriteLoc
  | ATuplePut ReadLoc
  | ATupleGetEl ReadLoc
                ReadLoc
                WriteLoc
  | AComment String
  deriving (Show)

comment ∷ Show a ⇒ a → UAsmOp
comment x = AComment $ show x

ret ∷ Integer → UAsmOp
ret = ARet

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
