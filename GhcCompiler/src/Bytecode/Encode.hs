module Bytecode.Encode
  ( toCompactUint
  , toCompactSint
  , toCompactReadLocM
  , toCompactLiteralM
  , toCompactLabelLocM
  , toCompactCodeLocM
  , toCompactWriteLoc
  , toCompactBool
  ) where

import           Asm
import           Bytecode.Bits
import           Bytecode.Encode.Const
import           Bytecode.Mod
import           Term

import qualified Control.Monad.State   as S

-- Produce untagged unsigned integer with 2 bits size prefix (4-8-16-32 bits)
toCompactUint :: Int -> BitStringList
toCompactUint u
  | u >= 0 && u < varlengthLimit0 = [bitsUB 0 2, bitsUB u varlength0]
  | u >= 0 && u < varlengthLimit1 = [bitsUB 1 2, bitsUB u varlength1]
  | u >= 0 && u < varlengthLimit2 = [bitsUB 2 2, bitsUB u varlength2]
  | u >= 0 && u < varlengthLimit3 = [bitsUB 3 2, bitsUB u varlength3]

-- Produce untagged signed integer with 2 bits size prefix (4-8-16-32 bits)
toCompactSint :: Int -> BitStringList
toCompactSint s
  | signedFitsIn s varlength0 = [bitsUB 0 2, bitsSB s varlength0]
  | signedFitsIn s varlength1 = [bitsUB 1 2, bitsSB s varlength1]
  | signedFitsIn s varlength2 = [bitsUB 2 2, bitsSB s varlength2]
  | signedFitsIn s varlength3 = [bitsUB 3 2, bitsSB s varlength3]

toCompactReadLocM :: ReadLoc -> S.State BcModule BitStringList
toCompactReadLocM (RRegX x) = return $ termTag termTagRegX : toCompactUint x
toCompactReadLocM (RRegY y) = return $ termTag termTagRegY : toCompactUint y
toCompactReadLocM RNil = return [termTag termTagNil]
toCompactReadLocM (RInt i) = do
  let limI = fromIntegral i -- todo bigint support?
  return $ termTag termTagInteger : toCompactSint limI
toCompactReadLocM (RAtom a) = do
  aIndex <- bcmFindAddAtomM a
  return $ termTag termTagAtom : toCompactUint aIndex
toCompactReadLocM (RLit lit) = toCompactLiteralM lit

-- [monadic] Update literal table if needed. Return index in the literal table
toCompactLiteralM :: Term -> S.State BcModule [BitString]
toCompactLiteralM lit = do
  litIndex <- bcmFindAddLiteralM lit
  return $ termTag termTagLiteral : toCompactUint litIndex

toCompactWriteLoc :: WriteLoc -> BitStringList
toCompactWriteLoc (WRegX x) = termTag termTagRegX : toCompactUint x
toCompactWriteLoc (WRegY y) = termTag termTagRegY : toCompactUint y
toCompactWriteLoc WIgnore   = [termTag termTagNil]

-- [monadic] Encode code location as label, no label or an import (updates
-- import table in the module if needed)
toCompactCodeLocM :: CodeLoc -> S.State BcModule BitStringList
toCompactCodeLocM (CLabel lloc) = toCompactLabelLocM lloc
toCompactCodeLocM (CExtFunc m f a) = toCompactLiteralM lit -- do as import?
  where
    lit = ErlTuple [Atom m, Atom f, ErlInt (toInteger a)]

toCompactLabelLocM :: LabelLoc -> S.State BcModule BitStringList
toCompactLabelLocM (LabelLoc i) = return $ toCompactUint i
toCompactLabelLocM UNoLabel     = return [termTag termTagNil]

toCompactBool :: Bool -> BitString
toCompactBool True  = bitsUB 1 1
toCompactBool False = bitsUB 0 1
