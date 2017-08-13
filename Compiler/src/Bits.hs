-- Module provides facilities for compact encoding of tagged values in sub-byte
-- units. Terms have a 3-bit tag. Integers have a 2-bit length tag choosing
-- between 4-8-16 or 32 bit length. Reader/writer assume whether integer has
-- a sign or not: this information is not stored.
module Bits
  ( Bits
  , BitsList
  , bitsUB
  , bitsSB
  , signedFitsIn
  , makeBits
  ) where

import qualified Data.Bits as DBits
import qualified Data.List as L
import qualified Data.ByteString as DBS

-- An unsigned Word value which fits into Word8 bits
data Bits
  = BitsSignedBig Int
                  Int
  | BitsUnsignedBig Int
                    Int
  | Bytes DBS.ByteString

instance Show Bits where
  show (BitsSignedBig v bits)   = show v ++ ":" ++ show bits ++ "/signed"
  show (BitsUnsignedBig v bits) = show v ++ ":" ++ show bits
  show (Bytes bs) = "\"" ++ show bs ++ "\""

-- A sequence of non-byte-aligned bits which will be joined together.
-- Assuming the reader is aware what bits mean and can decode this sequence.
type BitsList = [Bits]

signedFitsIn :: (DBits.Bits a, Num a, Ord a) => a -> Int -> Bool
signedFitsIn svalue bits =
  let limit = 1 `DBits.shiftL` (bits - 1)
  in svalue >= -limit && svalue <= limit - 1

bitsUB :: Int -> Int -> Bits
bitsUB val bits
  | val >= 0 && val < (1 `DBits.shiftL` bits) = BitsUnsignedBig val bits

bitsSB :: Int -> Int -> Bits
bitsSB val bits
  | signedFitsIn val bits = BitsSignedBig val bits

-- Creates an unsigned integer from bit boolean sequence (no size preserved,
-- use original input length to know the bit size)
bitPack :: [Bool] -> Int
bitPack inp = outp
  where
    foldFn (val, pos) True = (val `DBits.setBit` pos, pos + 1)
    foldFn (val, pos) False = (val, pos + 1)
    (outp, _) = L.foldl foldFn (0, 0) inp

makeBits :: [Bool] -> Bits
makeBits val = bitsUB (bitPack val) (length val)
