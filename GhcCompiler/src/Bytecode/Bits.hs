-- Module provides facilities for compact encoding of tagged values in sub-byte
-- units. Terms have a 3-bit tag. Integers have a 2-bit length tag choosing
-- between 4-8-16 or 32 bit length. Reader/writer assume whether integer has
-- a sign or not: this information is not stored.
module Bytecode.Bits
  ( BitString
  , BitStringList
  , bitsUB
  , bitsSB
  , signedFitsIn
  ) where

import           Data.Bits

-- An unsigned Word value which fits into Word8 bits
data BitString
  = BitsSignedBig Int
                  Int
  | BitsUnsignedBig Int
                    Int

instance Show BitString where
  show (BitsSignedBig v bits)   = show v ++ ":" ++ show bits ++ "/signed"
  show (BitsUnsignedBig v bits) = show v ++ ":" ++ show bits

-- A sequence of non-byte-aligned bits which will be joined together.
-- Assuming the reader is aware what bits mean and can decode this sequence.
type BitStringList = [BitString]

signedFitsIn :: (Bits a, Num a, Ord a) => a -> Int -> Bool
signedFitsIn svalue bits =
  let limit = 1 `shiftL` (bits - 1)
  in svalue >= -limit && svalue <= limit - 1

bitsUB :: Int -> Int -> BitString
bitsUB val bits
  | val >= 0 && val < (1 `shiftL` bits) = BitsUnsignedBig val bits

bitsSB :: Int -> Int -> BitString
bitsSB val bits
  | signedFitsIn val bits = BitsSignedBig val bits
