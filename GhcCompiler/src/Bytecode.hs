module Bytecode
  ( alloc
  , callM
  , callBifM
  , deconsM
  , encodeAtomM
  , err
  , jump
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
import qualified Bytecode.Bits       as BB
import qualified Bytecode.Encode     as BE
import qualified Bytecode.Mod        as BM
import qualified Bytecode.Op         as BO
import qualified Uerlc

import qualified Control.Monad.State as S

encodeError :: A.BuiltinError -> BB.BitsList
encodeError A.EBadArg           = BE.encUint 0
encodeError (A.EBadMatch _rloc) = BE.encUint 1
encodeError A.ECaseClause       = BE.encUint 2
encodeError A.EFunClause        = BE.encUint 3
encodeError A.EIfClause         = BE.encUint 4

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
  argBits <- mapM BE.encReadLocM args
  let onfailBits =
        case onfail of
          A.LabelLoc onfailL -> BE.encBool True : BE.encUint onfailL
          A.NoLabel          -> [BE.encBool False]
      dstBits = BE.encWriteLoc dst
      liveBits =
        case maybeLive of
          Just l  -> BE.encBool True : BE.encUint l
          Nothing -> [BE.encBool False]
      opArgs =
        BE.encUint testNameAtom ++
        onfailBits ++ liveBits ++ dstBits ++ concat argBits
  return $ BO.Instruction BO.Test opArgs

alloc :: Int -> Int -> BO.Instruction
alloc need live = BO.Instruction BO.Alloc (bitsNeed ++ bitsLive)
  where
    bitsNeed = BE.encUint need
    bitsLive = BE.encUint live

testHeap :: Int -> Int -> BO.Instruction
testHeap need live = BO.Instruction BO.TestHeap (bitsNeed ++ bitsLive)
  where
    bitsNeed = BE.encUint need
    bitsLive = BE.encUint live

-- [monadic] Compile a move instruction. BM.Module state is updated if
-- readloc src contains an atom or literal index not yet in the module tables
moveM :: A.ReadLoc -> A.WriteLoc -> BM.ModuleState BO.Instruction
moveM src dst = do
  bitsSrc <- BE.encReadLocM src
  let bitsDst = BE.encWriteLoc dst
  return $ BO.Instruction BO.Move (bitsSrc ++ bitsDst)

callM :: Int -> A.CodeLoc -> A.CallType -> BM.ModuleState BO.Instruction
callM arity codeLoc callType = do
  let arityBits = BE.encUint arity
  locBits <- BE.encCodeLocM codeLoc
  let (opCode, ctypeBits) =
        case callType of
          A.NormalCall -> (BO.Call, [])
          A.TailCall -> (BO.CallTail, [])
          A.GcEnabledCall live -> (BO.CallGc, BE.encUint live)
          A.TailCallDealloc dealloc -> (BO.CallTailDealloc, BE.encUint dealloc)
  return $ BO.Instruction opCode (arityBits ++ locBits ++ ctypeBits)

tupleNewM :: Int -> A.WriteLoc -> BM.ModuleState BO.Instruction
tupleNewM sz dst = do
  let dstBits = BE.encWriteLoc dst
      szBits = BE.encUint sz
  return $ BO.Instruction BO.TupleNew (szBits ++ dstBits)

tuplePutM :: A.ReadLoc -> BM.ModuleState BO.Instruction
tuplePutM val = do
  valBits <- BE.encReadLocM val
  return $ BO.Instruction BO.TuplePut valBits

tupleGetElM ::
     A.ReadLoc -> A.ReadLoc -> A.WriteLoc -> BM.ModuleState BO.Instruction
tupleGetElM src i dst = do
  bitsSrc <- BE.encReadLocM src
  bitsI <- BE.encReadLocM i
  let bitsDst = BE.encWriteLoc dst
  return $ BO.Instruction BO.TupleGetEl (bitsSrc ++ bitsI ++ bitsDst)

tupleSetElM ::
     A.ReadLoc -> A.ReadLoc -> A.WriteLoc -> BM.ModuleState BO.Instruction
tupleSetElM val index dst = do
  bitsVal <- BE.encReadLocM val
  bitsI <- BE.encReadLocM index
  let bitsDst = BE.encWriteLoc dst
  return $ BO.Instruction BO.TupleSetEl (bitsVal ++ bitsI ++ bitsDst)

ret :: Int -> BO.Instruction
ret 0 = BO.Instruction BO.Ret0 []
ret dealloc =
  let bitsD = BE.encUint dealloc
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
  let bitsName = BE.encUint nameAIndex
      bitsFail = BE.encLabelLoc onfail
  bitsArgs <- mapM BE.encReadLocM args
  let bitsDst = BE.encWriteLoc dst
  let op =
        case callType of
          A.NormalCall         -> BO.CallBif
          A.GcEnabledCall live -> BO.CallBifGc
  return $
    BO.Instruction op (bitsName ++ bitsFail ++ concat bitsArgs ++ bitsDst)

deconsM ::
     A.ReadLoc -> A.WriteLoc -> A.WriteLoc -> BM.ModuleState BO.Instruction
deconsM src dstH dstT = do
  bitsSrc <- BE.encReadLocM src
  let bitsH = BE.encWriteLoc dstH
      bitsT = BE.encWriteLoc dstT
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
  bitsVal <- BE.encReadLocM val
  let bitsFail = BE.encLabelLoc onfail
  bitsJt <- BE.encJtabM jtab
  return $ BO.Instruction op (bitsVal ++ bitsFail ++ bitsJt)

jump :: A.LabelLoc -> BO.Instruction
jump lbl =
  let bitsLbl = BE.encLabelLoc lbl
  in BO.Instruction BO.Jump bitsLbl
