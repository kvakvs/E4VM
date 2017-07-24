-- Module provides facilities for compact encoding of tagged values in sub-byte
-- units. Terms have a 3-bit tag. Integers have a 2-bit length tag choosing
-- between 4-8-16 or 32 bit length. Reader/writer assume whether integer has
-- a sign or not: this information is not stored.
module Bytecode.Bits where

import           Data.Bits
import           Data.Word

-- An unsigned Word value which fits into Word8 bits
data BitString =
  BitString Int
            Int

instance Show BitString where
  show (BitString v bits) = show v ++ ":" ++ show bits

-- A sequence of non-byte-aligned bits which will be joined together.
-- Assuming the reader is aware what bits mean and can decode this sequence.
type BitStringList = [BitString]

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
  | u >= 0 && u < varlengthLimit0 = [BitString 0 2, BitString u varlength0]
  | u >= 0 && u < varlengthLimit1 = [BitString 1 2, BitString u varlength1]
  | u >= 0 && u < varlengthLimit2 = [BitString 2 2, BitString u varlength2]
  | u >= 0 && u < varlengthLimit3 = [BitString 3 2, BitString u varlength3]
