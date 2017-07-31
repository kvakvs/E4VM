module Bytecode
  ( alloc
  , callM
  , callBifM
  , deconsM
  , encodeAtomM
  , err
  , moveM
  , ret
  , selectM
  , testHeap
  , testM
  , tupleGetElM
  , tupleNewM
  , tuplePutM
  , tupleSetElM
  ) where

import qualified Asm                 as A
import           Bytecode.Bits
import qualified Bytecode.Encode     as BE
import qualified Bytecode.Mod        as BM
import qualified Bytecode.Op         as BO
import qualified Uerlc

import qualified Control.Monad.State as S

encodeError :: A.BuiltinError -> BitStringList
encodeError A.EBadArg           = BE.toCompactUint 0
encodeError (A.EBadMatch _rloc) = BE.toCompactUint 1
encodeError A.ECaseClause       = BE.toCompactUint 2
encodeError A.EFunClause        = BE.toCompactUint 3
encodeError A.EIfClause         = BE.toCompactUint 4

-- [monadic] Returns int index of an atom in the module atoms table, optionally
-- updates the atoms table if the string did not exist
encodeAtomM :: String -> S.State BM.Module Int
encodeAtomM a = do
  mod0 <- S.get
  let (mod1, index) =
        case BM.bcmFindAtom mod0 a of
          Just i  -> (mod0, i)
          Nothing -> BM.bcmAddAtom mod0 a
  S.put mod1
  return index

err :: A.BuiltinError -> BO.BcOp
err e = BO.BcOp BO.BcOpError (encodeError e)

-- [monadic] Updates atom table if needed, and returns atom index for a string
testM ::
     String
  -> A.LabelLoc
  -> [A.ReadLoc]
  -> Maybe Int
  -> A.WriteLoc
  -> S.State BM.Module BO.BcOp
testM tname onfail args maybeLive dst = do
  testNameAtom <- encodeAtomM tname
  argBits <- mapM BE.toCompactReadLocM args
  let onfailBits =
        case onfail of
          A.LabelLoc onfailL -> BE.toCompactBool True : BE.toCompactUint onfailL
          A.UNoLabel         -> [BE.toCompactBool False]
      dstBits = BE.toCompactWriteLoc dst
      liveBits =
        case maybeLive of
          Just l  -> BE.toCompactBool True : BE.toCompactUint l
          Nothing -> [BE.toCompactBool False]
      opArgs =
        BE.toCompactUint testNameAtom ++
        onfailBits ++ liveBits ++ dstBits ++ concat argBits
  return $ BO.BcOp BO.BcOpTest opArgs

alloc :: Int -> Int -> BO.BcOp
alloc need live = BO.BcOp BO.BcOpAlloc (bitsNeed ++ bitsLive)
  where
    bitsNeed = BE.toCompactUint need
    bitsLive = BE.toCompactUint live

testHeap :: Int -> Int -> BO.BcOp
testHeap need live = BO.BcOp BO.BcOpTestHeap (bitsNeed ++ bitsLive)
  where
    bitsNeed = BE.toCompactUint need
    bitsLive = BE.toCompactUint live

-- [monadic] Compile a move instruction. BM.Module state is updated if
-- readloc src contains an atom or literal index not yet in the module tables
moveM :: A.ReadLoc -> A.WriteLoc -> S.State BM.Module BO.BcOp
moveM src dst = do
  bitsSrc <- BE.toCompactReadLocM src
  let bitsDst = BE.toCompactWriteLoc dst
  return $ BO.BcOp BO.BcOpMove (bitsSrc ++ bitsDst)

callM :: Int -> A.CodeLoc -> A.UCallType -> S.State BM.Module BO.BcOp
callM arity codeLoc callType = do
  let arityBits = BE.toCompactUint arity
  locBits <- BE.toCompactCodeLocM codeLoc
  let (opCode, ctypeBits) =
        case callType of
          A.NormalCall -> (BO.BcOpCallNormal, [])
          A.TailCall -> (BO.BcOpCallTail, [])
          A.GcEnabledCall live -> (BO.BcOpCallGc, BE.toCompactUint live)
          A.TailCallDealloc dealloc ->
            (BO.BcOpCallTailDealloc, BE.toCompactUint dealloc)
  return $ BO.BcOp opCode (arityBits ++ locBits ++ ctypeBits)

tupleNewM :: Int -> A.WriteLoc -> S.State BM.Module BO.BcOp
tupleNewM sz dst = do
  let dstBits = BE.toCompactWriteLoc dst
      szBits = BE.toCompactUint sz
  return $ BO.BcOp BO.BcOpTupleNew (szBits ++ dstBits)

tuplePutM :: A.ReadLoc -> S.State BM.Module BO.BcOp
tuplePutM val = do
  valBits <- BE.toCompactReadLocM val
  return $ BO.BcOp BO.BcOpTuplePut valBits

tupleGetElM :: A.ReadLoc -> A.ReadLoc -> A.WriteLoc -> S.State BM.Module BO.BcOp
tupleGetElM src i dst = do
  bitsSrc <- BE.toCompactReadLocM src
  bitsI <- BE.toCompactReadLocM i
  let bitsDst = BE.toCompactWriteLoc dst
  return $ BO.BcOp BO.BcOpTupleGetEl (bitsSrc ++ bitsI ++ bitsDst)

tupleSetElM :: A.ReadLoc -> A.ReadLoc -> A.WriteLoc -> S.State BM.Module BO.BcOp
tupleSetElM val index dst = do
  bitsVal <- BE.toCompactReadLocM val
  bitsI <- BE.toCompactReadLocM index
  let bitsDst = BE.toCompactWriteLoc dst
  return $ BO.BcOp BO.BcOpTupleSetEl (bitsVal ++ bitsI ++ bitsDst)

ret :: Int -> BO.BcOp
ret 0 = BO.BcOp BO.BcOpRet0 []
ret dealloc =
  let bitsD = BE.toCompactUint dealloc
  in BO.BcOp BO.BcOpRetN bitsD

callBifM ::
     String
  -> A.LabelLoc
  -> [A.ReadLoc]
  -> A.UCallType
  -> A.WriteLoc
  -> S.State BM.Module BO.BcOp
callBifM name onfail args callType dst = do
  nameAIndex <- BM.bcmFindAddAtomM name
  let bitsName = BE.toCompactUint nameAIndex
  bitsFail <- BE.toCompactLabelLocM onfail
  bitsArgs <- mapM BE.toCompactReadLocM args
  let bitsDst = BE.toCompactWriteLoc dst
  let op =
        case callType of
          A.NormalCall         -> BO.BcOpCallBif
          A.GcEnabledCall live -> BO.BcOpCallBifGc
  return $ BO.BcOp op (bitsName ++ bitsFail ++ concat bitsArgs ++ bitsDst)

deconsM :: A.ReadLoc -> A.WriteLoc -> A.WriteLoc -> S.State BM.Module BO.BcOp
deconsM src dstH dstT = do
  bitsSrc <- BE.toCompactReadLocM src
  let bitsH = BE.toCompactWriteLoc dstH
      bitsT = BE.toCompactWriteLoc dstT
  return $ BO.BcOp BO.BcOpDecons (bitsSrc ++ bitsH ++ bitsT)

selectM ::
     A.SelectSubj
  -> A.ReadLoc
  -> A.LabelLoc
  -> A.JumpTab
  -> S.State BM.Module BO.BcOp
selectM selType val onfail jtab = do
  let op =
        case selType of
          A.SValue      -> BO.BcOpSelectVal
          A.STupleArity -> BO.BcOpSelectTupleArity
  bitsVal <- BE.toCompactReadLocM val
  bitsFail <- BE.toCompactLabelLocM onfail
  bitsJt <- BE.toCompactJumptableM jtab
  return $ BO.BcOp op (bitsVal ++ bitsFail ++ bitsJt)
