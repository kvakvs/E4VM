module Asm.Binary
  ( BinaryFlags(..)
  ) where

data BinUnitWidth =
  BinUnitWidth Int
               Int

-- options for binary value: (unitsize, signed, bigendian)
data BinaryFlags = BinaryFlags
  { bfUnitSize :: Int
  , bfSigned :: Bool
  , bfBig :: Bool
  }

instance Show BinaryFlags where
  show (BinaryFlags u sig big) =
    "unit:" ++ show u ++ "/" ++ sigStr ++ "/" ++ bigStr
    where
      sigStr =
        if sig
          then "signed"
          else "unsigned"
      bigStr =
        if big
          then "big"
          else "little"
