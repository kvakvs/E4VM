module Bytecode.Encode where

import           Asm
import           Bytecode.Bits
import           Bytecode.Encode.Const
import           Bytecode.Mod

import qualified Control.Monad.State   as S
import           Data.Bits

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

toCompactReadLoc :: ReadLoc -> S.State BcModule BitStringList
toCompactReadLoc (RRegX x) = return $ termTag termTagRegX : toCompactUint x
toCompactReadLoc (RRegY y) = return $ termTag termTagRegY : toCompactUint y
toCompactReadLoc RNil = return [termTag termTagNil]
toCompactReadLoc (RInt i) = do
  let limI = fromIntegral i -- todo bigint support?
  return $ termTag termTagInteger : toCompactSint limI
toCompactReadLoc (RAtom a) = do
  mod0 <- S.get
  let Just aIndex = bcmFindAtom mod0 a
  return $ termTag termTagAtom : toCompactUint aIndex
toCompactReadLoc (RLit lit) = do
  mod0 <- S.get
  let Just litIndex = bcmFindLiteral mod0 lit
  return $ termTag termTagLiteral : toCompactUint litIndex

toCompactWriteLoc :: WriteLoc -> BitStringList
toCompactWriteLoc (WRegX x) = termTag termTagRegX : toCompactUint x
toCompactWriteLoc (WRegY y) = termTag termTagRegY : toCompactUint y
toCompactWriteLoc WIgnore   = [termTag termTagNil]

toCompactBool :: Bool -> BitString
toCompactBool True  = bitsUB 1 1
toCompactBool False = bitsUB 0 1
