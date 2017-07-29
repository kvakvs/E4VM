module Bytecode
  ( alloc
  , callM
  , callBifM
  , encodeAtomM
  , err
  , moveM
  , ret
  , testHeap
  , testM
  , tupleGetElM
  , tupleNewM
  , tuplePutM
  , tupleSetElM
  ) where

import qualified Asm
import           Bytecode.Bits
import           Bytecode.Encode
import           Bytecode.Mod
import           Bytecode.Op
import qualified Uerlc

import qualified Control.Monad.State as S

encodeError :: Asm.BuiltinError -> BitStringList
encodeError Asm.EBadArg           = toCompactUint 0
encodeError (Asm.EBadMatch _rloc) = toCompactUint 1
encodeError Asm.ECaseClause       = toCompactUint 2
encodeError Asm.EFunClause        = toCompactUint 3
encodeError Asm.EIfClause         = toCompactUint 4

-- [monadic] Returns int index of an atom in the module atoms table, optionally
-- updates the atoms table if the string did not exist
encodeAtomM :: String -> S.State BcModule Int
encodeAtomM a = do
  mod0 <- S.get
  let (mod1, index) =
        case bcmFindAtom mod0 a of
          Just i  -> (mod0, i)
          Nothing -> bcmAddAtom mod0 a
  S.put mod1
  return index

err :: Asm.BuiltinError -> BcOp
err e = BcOp BcOpError (encodeError e)

-- [monadic] Updates atom table if needed, and returns atom index for a string
testM ::
     String
  -> Asm.LabelLoc
  -> [Asm.ReadLoc]
  -> Maybe Int
  -> Asm.WriteLoc
  -> S.State BcModule BcOp
testM tname onfail args maybeLive dst = do
  testNameAtom <- encodeAtomM tname
  argBits <- mapM toCompactReadLocM args
  let onfailBits =
        case onfail of
          Asm.LabelLoc onfailL -> toCompactBool True : toCompactUint onfailL
          Asm.UNoLabel         -> [toCompactBool False]
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

testHeap :: Int -> Int -> BcOp
testHeap need live = BcOp BcOpTestHeap (bitsNeed ++ bitsLive)
  where
    bitsNeed = toCompactUint need
    bitsLive = toCompactUint live

-- [monadic] Compile a move instruction. BcModule state is updated if
-- readloc src contains an atom or literal index not yet in the module tables
moveM :: Asm.ReadLoc -> Asm.WriteLoc -> S.State BcModule BcOp
moveM src dst = do
  bitsSrc <- toCompactReadLocM src
  let bitsDst = toCompactWriteLoc dst
  return $ BcOp BcOpMove (bitsSrc ++ bitsDst)

callM :: Int -> Asm.CodeLoc -> Asm.UCallType -> S.State BcModule BcOp
callM arity codeLoc callType = do
  let arityBits = toCompactUint arity
  locBits <- toCompactCodeLocM codeLoc
  let (opCode, ctypeBits) =
        case callType of
          Asm.NormalCall -> (BcOpCallNormal, [])
          Asm.TailCall -> (BcOpCallTail, [])
          Asm.GcEnabledCall live -> (BcOpCallGc, toCompactUint live)
          Asm.TailCallDealloc dealloc ->
            (BcOpCallTailDealloc, toCompactUint dealloc)
  return $ BcOp opCode (arityBits ++ locBits ++ ctypeBits)

tupleNewM :: Int -> Asm.WriteLoc -> S.State BcModule BcOp
tupleNewM sz dst = do
  let dstBits = toCompactWriteLoc dst
      szBits = toCompactUint sz
  return $ BcOp BcOpTupleNew (szBits ++ dstBits)

tuplePutM :: Asm.ReadLoc -> S.State BcModule BcOp
tuplePutM val = do
  valBits <- toCompactReadLocM val
  return $ BcOp BcOpTuplePut valBits

tupleGetElM ::
     Asm.ReadLoc -> Asm.ReadLoc -> Asm.WriteLoc -> S.State BcModule BcOp
tupleGetElM src i dst = do
  bitsSrc <- toCompactReadLocM src
  bitsI <- toCompactReadLocM i
  let bitsDst = toCompactWriteLoc dst
  return $ BcOp BcOpTupleGetEl (bitsSrc ++ bitsI ++ bitsDst)

tupleSetElM ::
     Asm.ReadLoc -> Asm.ReadLoc -> Asm.WriteLoc -> S.State BcModule BcOp
tupleSetElM val index dst = do
  bitsVal <- toCompactReadLocM val
  bitsI <- toCompactReadLocM index
  let bitsDst = toCompactWriteLoc dst
  return $ BcOp BcOpTupleSetEl (bitsVal ++ bitsI ++ bitsDst)

ret :: Int -> BcOp
ret 0 = BcOp BcOpRet0 []
ret dealloc =
  let bitsD = toCompactUint dealloc
  in BcOp BcOpRetN bitsD

callBifM name onfail args callType dst = do
  nameAIndex <- bcmFindAddAtomM name
  let bitsName = toCompactUint nameAIndex
  bitsFail <- toCompactLabelLocM onfail
  bitsArgs <- mapM toCompactReadLocM args
  let bitsDst = toCompactWriteLoc dst
  let op =
        case callType of
          Asm.NormalCall -> BcOpCallBif
          Asm.GcEnabledCall live -> BcOpCallBifGc
  return $ BcOp op (bitsName ++ bitsFail ++ concat bitsArgs ++ bitsDst)
