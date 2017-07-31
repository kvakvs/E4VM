module Bytecode.Encode
  ( toCompactBool
  , toCompactCodeLocM
  , toCompactJumptabM
  , toCompactLabelLocM
  , toCompactLiteralM
  , toCompactReadLocM
  , toCompactSint
  , toCompactUint
  , toCompactWriteLoc
  ) where

import qualified Asm                   as A
import           Bytecode.Bits
import           Bytecode.Encode.Const
import qualified Bytecode.Mod          as BM
import qualified Term                  as T

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

toCompactReadLocM :: A.ReadLoc -> BM.ModuleState BitStringList
toCompactReadLocM (A.RRegX x) = return $ termTag termTagRegX : toCompactUint x
toCompactReadLocM (A.RRegY y) = return $ termTag termTagRegY : toCompactUint y
toCompactReadLocM A.RNil = return [termTag termTagNil]
toCompactReadLocM (A.RInt i) = do
  let limI = fromIntegral i -- todo bigint support?
  return $ termTag termTagInteger : toCompactSint limI
toCompactReadLocM (A.RAtom a) = do
  aIndex <- BM.findAddAtomM a
  return $ termTag termTagAtom : toCompactUint aIndex
toCompactReadLocM (A.RLit lit) = toCompactLiteralM lit

-- [monadic] Update literal table if needed. Return index in the literal table
toCompactLiteralM :: T.Term -> BM.ModuleState [BitString]
toCompactLiteralM lit = do
  litIndex <- BM.findAddLitM lit
  return $ termTag termTagLiteral : toCompactUint litIndex

toCompactWriteLoc :: A.WriteLoc -> BitStringList
toCompactWriteLoc (A.WRegX x) = termTag termTagRegX : toCompactUint x
toCompactWriteLoc (A.WRegY y) = termTag termTagRegY : toCompactUint y
toCompactWriteLoc A.WIgnore   = [termTag termTagNil]

-- [monadic] Encode code location as label, no label or an import (updates
-- import table in the module if needed)
toCompactCodeLocM :: A.CodeLoc -> BM.ModuleState BitStringList
toCompactCodeLocM (A.CLabel lloc) = toCompactLabelLocM lloc
toCompactCodeLocM (A.CExtFunc m f a) = toCompactLiteralM lit -- do as import?
  where
    lit = T.ErlTuple [T.Atom m, T.Atom f, T.ErlInt (toInteger a)]

toCompactLabelLocM :: A.LabelLoc -> BM.ModuleState BitStringList
toCompactLabelLocM (A.LabelLoc i) = return $ toCompactUint i
toCompactLabelLocM A.NoLabel     = return [termTag termTagNil]

toCompactBool :: Bool -> BitString
toCompactBool True  = bitsUB 1 1
toCompactBool False = bitsUB 0 1

toCompactJumptabM :: A.JumpTab -> BM.ModuleState BitStringList
toCompactJumptabM jtab = do
  jtIndex <- BM.addJumptabM jtab
  return $ toCompactUint jtIndex
