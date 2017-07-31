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
encodeAtomM :: String -> BM.ModuleState Int
encodeAtomM a = do
  mod0 <- S.get
  let (mod1, index) =
        case BM.findAtom mod0 a of
          Just i  -> (mod0, i)
          Nothing -> BM.addAtom mod0 a
  S.put mod1
  return index

-- Create an instruction to generate exception
err :: A.BuiltinError -> BO.Instruction
err e = BO.Instruction BO.Error (encodeError e)

-- [monadic] Updates atom table if needed, and returns atom index for a string
testM ::
     String
  -> A.LabelLoc
  -> [A.ReadLoc]
  -> Maybe Int
  -> A.WriteLoc
  -> BM.ModuleState BO.Instruction
testM tname onfail args maybeLive dst = do
  testNameAtom <- encodeAtomM tname
  argBits <- mapM BE.toCompactReadLocM args
  let onfailBits =
        case onfail of
          A.LabelLoc onfailL -> BE.toCompactBool True : BE.toCompactUint onfailL
          A.NoLabel -> [BE.toCompactBool False]
      dstBits = BE.toCompactWriteLoc dst
      liveBits =
        case maybeLive of
          Just l  -> BE.toCompactBool True : BE.toCompactUint l
          Nothing -> [BE.toCompactBool False]
      opArgs =
        BE.toCompactUint testNameAtom ++
        onfailBits ++ liveBits ++ dstBits ++ concat argBits
  return $ BO.Instruction BO.Test opArgs

alloc :: Int -> Int -> BO.Instruction
alloc need live = BO.Instruction BO.Alloc (bitsNeed ++ bitsLive)
  where
    bitsNeed = BE.toCompactUint need
    bitsLive = BE.toCompactUint live

testHeap :: Int -> Int -> BO.Instruction
testHeap need live = BO.Instruction BO.TestHeap (bitsNeed ++ bitsLive)
  where
    bitsNeed = BE.toCompactUint need
    bitsLive = BE.toCompactUint live

-- [monadic] Compile a move instruction. BM.Module state is updated if
-- readloc src contains an atom or literal index not yet in the module tables
moveM :: A.ReadLoc -> A.WriteLoc -> BM.ModuleState BO.Instruction
moveM src dst = do
  bitsSrc <- BE.toCompactReadLocM src
  let bitsDst = BE.toCompactWriteLoc dst
  return $ BO.Instruction BO.Move (bitsSrc ++ bitsDst)

callM :: Int -> A.CodeLoc -> A.CallType -> BM.ModuleState BO.Instruction
callM arity codeLoc callType = do
  let arityBits = BE.toCompactUint arity
  locBits <- BE.toCompactCodeLocM codeLoc
  let (opCode, ctypeBits) =
        case callType of
          A.NormalCall -> (BO.Call, [])
          A.TailCall -> (BO.CallTail, [])
          A.GcEnabledCall live -> (BO.CallGc, BE.toCompactUint live)
          A.TailCallDealloc dealloc ->
            (BO.CallTailDealloc, BE.toCompactUint dealloc)
  return $ BO.Instruction opCode (arityBits ++ locBits ++ ctypeBits)

tupleNewM :: Int -> A.WriteLoc -> BM.ModuleState BO.Instruction
tupleNewM sz dst = do
  let dstBits = BE.toCompactWriteLoc dst
      szBits = BE.toCompactUint sz
  return $ BO.Instruction BO.TupleNew (szBits ++ dstBits)

tuplePutM :: A.ReadLoc -> BM.ModuleState BO.Instruction
tuplePutM val = do
  valBits <- BE.toCompactReadLocM val
  return $ BO.Instruction BO.TuplePut valBits

tupleGetElM ::
     A.ReadLoc -> A.ReadLoc -> A.WriteLoc -> BM.ModuleState BO.Instruction
tupleGetElM src i dst = do
  bitsSrc <- BE.toCompactReadLocM src
  bitsI <- BE.toCompactReadLocM i
  let bitsDst = BE.toCompactWriteLoc dst
  return $ BO.Instruction BO.TupleGetEl (bitsSrc ++ bitsI ++ bitsDst)

tupleSetElM ::
     A.ReadLoc -> A.ReadLoc -> A.WriteLoc -> BM.ModuleState BO.Instruction
tupleSetElM val index dst = do
  bitsVal <- BE.toCompactReadLocM val
  bitsI <- BE.toCompactReadLocM index
  let bitsDst = BE.toCompactWriteLoc dst
  return $ BO.Instruction BO.TupleSetEl (bitsVal ++ bitsI ++ bitsDst)

ret :: Int -> BO.Instruction
ret 0 = BO.Instruction BO.Ret0 []
ret dealloc =
  let bitsD = BE.toCompactUint dealloc
  in BO.Instruction BO.RetN bitsD

callBifM ::
     String
  -> A.LabelLoc
  -> [A.ReadLoc]
  -> A.CallType
  -> A.WriteLoc
  -> BM.ModuleState BO.Instruction
callBifM name onfail args callType dst = do
  nameAIndex <- BM.findAddAtomM name
  let bitsName = BE.toCompactUint nameAIndex
  bitsFail <- BE.toCompactLabelLocM onfail
  bitsArgs <- mapM BE.toCompactReadLocM args
  let bitsDst = BE.toCompactWriteLoc dst
  let op =
        case callType of
          A.NormalCall         -> BO.CallBif
          A.GcEnabledCall live -> BO.CallBifGc
  return $
    BO.Instruction op (bitsName ++ bitsFail ++ concat bitsArgs ++ bitsDst)

deconsM ::
     A.ReadLoc -> A.WriteLoc -> A.WriteLoc -> BM.ModuleState BO.Instruction
deconsM src dstH dstT = do
  bitsSrc <- BE.toCompactReadLocM src
  let bitsH = BE.toCompactWriteLoc dstH
      bitsT = BE.toCompactWriteLoc dstT
  return $ BO.Instruction BO.Decons (bitsSrc ++ bitsH ++ bitsT)

selectM ::
     A.SelectSubj
  -> A.ReadLoc
  -> A.LabelLoc
  -> A.JumpTab
  -> BM.ModuleState BO.Instruction
selectM selType val onfail jtab = do
  let op =
        case selType of
          A.SValue      -> BO.SelectVal
          A.STupleArity -> BO.SelectTupleArity
  bitsVal <- BE.toCompactReadLocM val
  bitsFail <- BE.toCompactLabelLocM onfail
  bitsJt <- BE.toCompactJumptabM jtab
  return $ BO.Instruction op (bitsVal ++ bitsFail ++ bitsJt)
