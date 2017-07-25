-- Module provides facilities for compact encoding of tagged values in sub-byte
-- units. Terms have a 3-bit tag. Integers have a 2-bit length tag choosing
-- between 4-8-16 or 32 bit length. Reader/writer assume whether integer has
-- a sign or not: this information is not stored.
module Bytecode.Bits where

-- An unsigned Word value which fits into Word8 bits
data BitString =
  BitsSignedBig Int Int |
  BitsUnsignedBig Int Int

instance Show BitString where
  show (BitsSignedBig v bits) = show v ++ ":" ++ show bits ++ "/signed"
  show (BitsUnsignedBig v bits) = show v ++ ":" ++ show bits

-- A sequence of non-byte-aligned bits which will be joined together.
-- Assuming the reader is aware what bits mean and can decode this sequence.
type BitStringList = [BitString]
