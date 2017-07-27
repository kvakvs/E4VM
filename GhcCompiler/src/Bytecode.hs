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
test ::
     String
  -> LabelLoc
  -> [ReadLoc]
  -> Maybe Int
  -> WriteLoc
  -> S.State BcModule BcOp
test tname onfail args maybeLive dst = do
  testNameAtom <- encodeAtom tname
  argBits <- mapM toCompactReadLocM args
  let onfailBits =
        case onfail of
          LabelLoc onfailL -> toCompactBool True : toCompactUint onfailL
          UNoLabel         -> [toCompactBool False]
      dstBits = toCompactWriteLoc dst
      liveBits =
        case maybeLive of
          Just l  -> toCompactBool True : toCompactUint l
          Nothing -> [toCompactBool False]
      opArgs =
        toCompactUint testNameAtom ++
        onfailBits ++ liveBits ++ dstBits ++ concat argBits
  return $ BcOp BcOpTest opArgs

alloc :: Int -> Int -> BcOp
alloc need live = BcOp BcOpAlloc (bitsNeed ++ bitsLive)
  where
    bitsNeed = toCompactUint need
    bitsLive = toCompactUint live

tupleGetEl :: ReadLoc -> ReadLoc -> WriteLoc -> S.State BcModule BcOp
tupleGetEl src i dst = do
  bitsSrc <- toCompactReadLocM src
  bitsI <- toCompactReadLocM i
  let bitsDst = toCompactWriteLoc dst
  return $ BcOp BcOpTGetEl (bitsSrc ++ bitsI ++ bitsDst)

-- [monadic] Compile a move instruction. BcModule state is updated if
-- readloc src contains an atom or literal index not yet in the module tables
move :: ReadLoc -> WriteLoc -> S.State BcModule BcOp
move src dst = do
  bitsSrc <- toCompactReadLocM src
  let bitsDst = toCompactWriteLoc dst
  return $ BcOp BcOpMove (bitsSrc ++ bitsDst)

call :: Int -> CodeLoc -> UCallType -> S.State BcModule BcOp
call arity codeLoc callType = do
  let arityBits = toCompactUint arity
  locBits <- toCompactCodeLocM codeLoc
  let (opCode, ctypeBits) =
        case callType of
          NormalCall -> (BcOpCallNormal, [])
          TailCall -> (BcOpCallTail, [])
          GcEnabledCall live -> (BcOpCallGc, toCompactUint live)
          TailCallDealloc dealloc ->
            (BcOpCallTailDealloc, toCompactUint dealloc)
  return $ BcOp opCode (arityBits ++ locBits ++ ctypeBits)
