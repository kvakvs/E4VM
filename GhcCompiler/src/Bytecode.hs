module Bytecode where

import           Asm
import           BytecodeMod
import           BytecodeOp

import qualified Control.Monad.State as S

encodeError :: Num t => BuiltinError -> [t]
encodeError EBadArg           = [0]
encodeError (EBadMatch _rloc) = [1]
encodeError ECaseClause       = [2]
encodeError EFunClause        = [3]
encodeError EIfClause         = [4]

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
test tname = do
  tnIndex <- encodeAtom tname
  return $ BcOp BcOpTest [tnIndex]
