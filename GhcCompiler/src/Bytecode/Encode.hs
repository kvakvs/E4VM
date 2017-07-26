module Bytecode.Encode where

import           Asm
import           Bytecode.Bits
import           Bytecode.Encode.Const

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
toCompactReadLoc (RRegX x) =
  BitsUnsignedBig termTagRegX termTag'BitSize : toCompactUint x

toCompactWriteLoc :: WriteLoc -> BitStringList
toCompactWriteLoc (WRegX x) =
  BitsUnsignedBig termTagRegX termTag'BitSize : toCompactUint x
