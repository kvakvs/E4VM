module Bytecode.Encode.Const where

import           Bytecode.Bits (bitsUB)
import           Data.Bits

varlength0 = 4 :: Int

varlength1 = 8 :: Int

varlength2 = 16 :: Int

varlength3 = 32 :: Int

varlengthLimit0 :: Int
varlengthLimit0 = 1 `shiftL` varlength0

varlengthLimit1 :: Int
varlengthLimit1 = 1 `shiftL` varlength1

varlengthLimit2 :: Int
varlengthLimit2 = 1 `shiftL` varlength2

varlengthLimit3 :: Int
varlengthLimit3 = 1 `shiftL` varlength3

termTag'BitSize = 3 :: Int

termTagRegX = 0 :: Int

termTagRegY = 1 :: Int

termTagNil = 2 :: Int

termTagAtom = 3 :: Int

termTagImport = 4 :: Int

termTagLambda = 5 :: Int

termTagLiteral = 6 :: Int

termTagInteger = 7 :: Int

termTag t = bitsUB t termTag'BitSize
