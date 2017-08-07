module Bitcode.Encode
  ( encBinaryFlags
  , encBool
  , encCodeLocM
  , encJtabM
  , encLabelLoc
  , encLitM
  , encReadLocM
  , encSint
  , encUint
  , encWriteLoc
  ) where

import qualified Asm                   as A
import qualified Bits                  as B
import           Bitcode.Encode.Const
import qualified Bitcode.Mod          as BM
import qualified Term                  as T

import qualified Control.Monad.State   as S
import qualified Data.Bits             as DBits

-- Produce untagged unsigned integer with 2 bits size prefix (4-8-16-32 bits)
encUint :: Int -> B.BitsList
encUint u
  | u >= 0 && u < varlengthLimit0 = [B.bitsUB 0 2, B.bitsUB u varlength0]
  | u >= 0 && u < varlengthLimit1 = [B.bitsUB 1 2, B.bitsUB u varlength1]
  | u >= 0 && u < varlengthLimit2 = [B.bitsUB 2 2, B.bitsUB u varlength2]
  | u >= 0 && u < varlengthLimit3 = [B.bitsUB 3 2, B.bitsUB u varlength3]

-- Produce untagged signed integer with 2 bits size prefix (4-8-16-32 bits)
encSint :: Int -> B.BitsList
encSint s
  | B.signedFitsIn s varlength0 = [B.bitsUB 0 2, B.bitsSB s varlength0]
  | B.signedFitsIn s varlength1 = [B.bitsUB 1 2, B.bitsSB s varlength1]
  | B.signedFitsIn s varlength2 = [B.bitsUB 2 2, B.bitsSB s varlength2]
  | B.signedFitsIn s varlength3 = [B.bitsUB 3 2, B.bitsSB s varlength3]

encReadLocM :: A.ReadLoc -> BM.ModuleState B.BitsList
encReadLocM (A.RRegX x) = return $ termTag termTagRegX : encUint x
encReadLocM (A.RRegY y) = return $ termTag termTagRegY : encUint y
encReadLocM A.RNil = return [termTag termTagNil]
encReadLocM (A.RInt i) = do
  let limI = fromIntegral i -- todo bigint support?
  return $ termTag termTagInteger : encSint limI
encReadLocM (A.RAtom a) = do
  aIndex <- BM.findAddAtomM a
  return $ termTag termTagAtom : encUint aIndex
encReadLocM (A.RLit lit) = encLitM lit
encReadLocM (A.RBinaryFlags bf) = return $ encBinaryFlags bf

encBinaryFlags :: A.BinaryFlags -> B.BitsList
encBinaryFlags (A.BinaryFlags unit sig big) = termTag termTagInteger : val
  where
    val = encUint $ (unit `DBits.shiftL` 2) + sigBit + bigBit
    sigBit =
      if sig
        then 2
        else 0
    bigBit =
      if big
        then 1
        else 0

-- Update literal table if needed. Return index in the literal table
encLitM :: T.Term -> BM.ModuleState [B.Bits]
encLitM lit = do
  litIndex <- BM.findAddLitM lit
  return $ termTag termTagLiteral : encUint litIndex

encWriteLoc :: A.WriteLoc -> B.BitsList
encWriteLoc (A.WRegX x) = termTag termTagRegX : encUint x
encWriteLoc (A.WRegY y) = termTag termTagRegY : encUint y
encWriteLoc A.WIgnore   = [termTag termTagNil]

-- Encode code location as label, no label or an import (updates
-- import table in the module if needed)
encCodeLocM :: A.CodeLoc -> BM.ModuleState B.BitsList
encCodeLocM (A.CLabel lloc) = return $ encLabelLoc lloc
encCodeLocM (A.CExtFunc m f a) = encLitM lit -- do as import?
  where
    lit = T.ErlTuple [T.Atom m, T.Atom f, T.ErlInt (toInteger a)]

encLabelLoc :: A.LabelLoc -> B.BitsList
encLabelLoc (A.LabelLoc i) = encUint i
encLabelLoc A.NoLabel      = [termTag termTagNil]

encBool :: Bool -> B.Bits
encBool True  = B.bitsUB 1 1
encBool False = B.bitsUB 0 1

encJtabM :: A.JumpTab -> BM.ModuleState B.BitsList
encJtabM jtab = do
  jtIndex <- BM.addJumptabM jtab
  return $ encUint jtIndex
