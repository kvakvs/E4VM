module Bitcode.Encode.Const where

import qualified Bits      as B

import           Data.Bits

varlength0 :: Int
varlength0 = 4 :: Int

varlength1 :: Int
varlength1 = 8 :: Int

varlength2 :: Int
varlength2 = 16 :: Int

varlength3 :: Int
varlength3 = 32 :: Int

varlengthLimit0 :: Int
varlengthLimit0 = 1 `shiftL` varlength0

varlengthLimit1 :: Int
varlengthLimit1 = 1 `shiftL` varlength1

varlengthLimit2 :: Int
varlengthLimit2 = 1 `shiftL` varlength2

varlengthLimit3 :: Int
varlengthLimit3 = 1 `shiftL` varlength3

termTag_bitSize :: Int
termTag_bitSize = 3 :: Int

termTagRegX :: Int
termTagRegX = 0 :: Int

termTagRegY :: Int
termTagRegY = 1 :: Int

termTagNil :: Int
termTagNil = 2 :: Int

termTagAtom :: Int
termTagAtom = 3 :: Int

--termTagImport :: Int
--termTagImport = 4 :: Int
termTagBinaryFlags :: Int
termTagBinaryFlags = 4 :: Int

--termTagLambda :: Int
--termTagLambda = 5 :: Int
termTagLiteral :: Int
termTagLiteral = 6 :: Int

termTagInteger :: Int
termTagInteger = 7 :: Int

termTag :: Int -> B.Bits
termTag t = B.bitsUB t termTag_bitSize
