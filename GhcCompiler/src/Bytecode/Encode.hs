module Bytecode.Encode where

import           Asm           (ReadLoc, WriteLoc)
import           Bytecode.Bits

import           Data.Bits

varlength0 :: Int
varlength0 = 4

varlength1 :: Int
varlength1 = 8

varlength2 :: Int
varlength2 = 16

varlength3 :: Int
varlength3 = 32

varlengthLimit0 :: Int
varlengthLimit0 = 1 `shiftL` varlength0

varlengthLimit1 :: Int
varlengthLimit1 = 1 `shiftL` varlength1

varlengthLimit2 :: Int
varlengthLimit2 = 1 `shiftL` varlength2

varlengthLimit3 :: Int
varlengthLimit3 = 1 `shiftL` varlength3

-- Produce untagged unsigned integer with 2 bits size prefix (4-8-16-32 bits)
toCompactUint :: Int -> BitStringList
toCompactUint u
  | u >= 0 && u < varlengthLimit0 =
    [BitsUnsignedBig 0 2, BitsUnsignedBig u varlength0]
  | u >= 0 && u < varlengthLimit1 =
    [BitsUnsignedBig 1 2, BitsUnsignedBig u varlength1]
  | u >= 0 && u < varlengthLimit2 =
    [BitsUnsignedBig 2 2, BitsUnsignedBig u varlength2]
  | u >= 0 && u < varlengthLimit3 =
    [BitsUnsignedBig 3 2, BitsUnsignedBig u varlength3]

toCompactReadLoc :: ReadLoc -> BitStringList
toCompactReadLoc (RRegX x) = []

toCompactWriteLoc :: WriteLoc -> BitStringList
toCompactWriteLoc s = []
