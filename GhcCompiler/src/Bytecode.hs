module Bytecode where

import           Asm
import           Bytecode.Bits
import           Bytecode.Encode
import           Bytecode.Mod
import           Bytecode.Op

import qualified Control.Monad.State as S

encodeError :: BuiltinError -> BitStringList
encodeError EBadArg           = toCompactUint 0
encodeError (EBadMatch _rloc) = toCompactUint 1
encodeError ECaseClause       = toCompactUint 2
encodeError EFunClause        = toCompactUint 3
encodeError EIfClause         = toCompactUint 4

-- [monadic] Returns int index of an atom in the module atoms table, optionally
-- updates the atoms table if the string did not exist
encodeAtom :: String -> S.State BcModule Int
encodeAtom a = do
  mod0 <- S.get
  let (mod1, index) =
        case bcmFindAtom mod0 a of
          Just i  -> (mod0, i)
          Nothing -> bcmAddAtom mod0 a
  S.put mod1
  return index

err :: BuiltinError -> BcOp
err e = BcOp BcOpError (encodeError e)

-- [monadic] Updates atom table if needed, and returns atom index for a string
test :: String -> S.State BcModule BcOp
test tname = do
  testNameAtom <- encodeAtom tname
  return $ BcOp BcOpTest (toCompactUint testNameAtom)

alloc :: Int -> Int -> BcOp
alloc need live = BcOp BcOpAlloc (bitsNeed ++ bitsLive)
  where
    bitsNeed = toCompactUint need
    bitsLive = toCompactUint live

tupleGetEl :: ReadLoc -> ReadLoc -> WriteLoc -> BcOp
tupleGetEl src i dst =
  BcOp BcOpTGetEl (bitsSrc ++ bitsI ++ bitsDst)
  where
      bitsSrc = toCompactReadLoc src
      bitsDst = toCompactWriteLoc dst
      bitsI = toCompactReadLoc i
