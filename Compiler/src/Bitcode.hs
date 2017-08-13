module Bitcode
  ( allocM
  , bsContextToBinM
  , bsInitM
  , bsPutIntegerM
  , bsRestoreM
  , bsSaveM
  , callBifM
  , callFunM
  , callM
  , consM
  , deconsM
  , encodeAtomM
  , invokeErrorM
  , jumpM
  , makeFunM
  , moveM
  , retM
  , selectM
  , setNilM
  , testHeapM
  , testM
  , trimM
  , tupleGetElM
  , tupleNewM
  , tuplePutM
  , tupleSetElM
  ) where

import qualified Asm                 as A
import qualified Bitcode.Encode      as BE
import qualified Bitcode.Mod         as BM
import qualified Bitcode.Op          as BO
import qualified Bits                as B
import           Uerlc

import qualified Control.Monad.State as S
import qualified Debug.Trace         as Dbg

type MResult = BM.ModuleState (CompileErrorOr [BO.Instruction])

encodeError :: A.BuiltinError -> B.BitsList
encodeError A.EBadArg           = BE.encUint 0
encodeError (A.EBadMatch _rloc) = BE.encUint 1
encodeError A.ECaseClause       = BE.encUint 2
encodeError A.EFunClause        = BE.encUint 3
encodeError A.EIfClause         = BE.encUint 4

-- Returns int index of an atom in the module atoms table, optionally
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

makeInstrM :: BO.Opcode -> B.BitsList -> BM.ModuleState [BO.Instruction]
makeInstrM op argBits = do
  let instr = BO.makeInstruction op argBits
  BM.profileOpcodeM op
  return [instr]

-- Create an instruction to generate exception
invokeErrorM :: A.BuiltinError -> MResult
invokeErrorM e = do
  instr <- makeInstrM BO.Error (encodeError e)
  return $ Right instr

-- Updates atom table if needed, and returns atom index for a string
testM ::
     String -> A.LabelLoc -> [A.ReadLoc] -> Maybe Int -> A.WriteLoc -> MResult
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
  instr <- makeInstrM BO.Test opArgs
  return $ Right instr

allocM :: Int -> Int -> MResult
allocM need live = do
  let bitsNeed = BE.encUint need
      bitsLive = BE.encUint live
  instr <- makeInstrM BO.Alloc (bitsNeed ++ bitsLive)
  return $ Right instr

testHeapM :: Int -> Int -> MResult
testHeapM need live = do
  let bitsNeed = BE.encUint need
      bitsLive = BE.encUint live
  instr <- makeInstrM BO.TestHeap (bitsNeed ++ bitsLive)
  return $ Right instr

-- Compile a move instruction. BM.Module state is updated if
-- readloc src contains an atom or literal index not yet in the module tables
moveM :: A.ReadLoc -> A.WriteLoc -> MResult
moveM src dst = do
  bitsSrc <- BE.encReadLocM src
  let bitsDst = BE.encWriteLoc dst
  instr <- makeInstrM BO.Move (bitsSrc ++ bitsDst)
  return $ Right instr

callM :: Int -> A.CodeLoc -> A.CallType -> MResult
callM arity codeLoc callType = do
  let arityBits = BE.encUint arity
  locBits <- BE.encCodeLocM codeLoc
  let (opCode, ctypeBits) =
        case callType of
          A.NormalCall -> (BO.Call, [])
          A.TailCall -> (BO.CallTail, [])
          A.GcEnabledCall live -> (BO.CallGc, BE.encUint live)
          A.TailCallDealloc dealloc -> (BO.CallTailDealloc, BE.encUint dealloc)
  instr <- makeInstrM opCode (arityBits ++ locBits ++ ctypeBits)
  return $ Right instr

tupleNewM :: Int -> A.WriteLoc -> MResult
tupleNewM sz dst = do
  let dstBits = BE.encWriteLoc dst
      szBits = BE.encUint sz
  instr <- makeInstrM BO.TupleNew (szBits ++ dstBits)
  return $ Right instr

tuplePutM :: A.ReadLoc -> MResult
tuplePutM val = do
  valBits <- BE.encReadLocM val
  instr <- makeInstrM BO.TuplePut valBits
  return $ Right instr

tupleGetElM :: A.ReadLoc -> A.ReadLoc -> A.WriteLoc -> MResult
tupleGetElM src i dst = do
  bitsSrc <- BE.encReadLocM src
  bitsI <- BE.encReadLocM i
  let bitsDst = BE.encWriteLoc dst
  instr <- makeInstrM BO.TupleGetEl (bitsSrc ++ bitsI ++ bitsDst)
  return $ Right instr

tupleSetElM :: A.ReadLoc -> A.ReadLoc -> A.WriteLoc -> MResult
tupleSetElM val index dst = do
  bitsVal <- BE.encReadLocM val
  bitsI <- BE.encReadLocM index
  let bitsDst = BE.encWriteLoc dst
  instr <- makeInstrM BO.TupleSetEl (bitsVal ++ bitsI ++ bitsDst)
  return $ Right instr

retM :: Int -> MResult
retM 0 = do
  instr <- makeInstrM BO.Ret0 []
  return $ Right instr
retM dealloc = do
  let bitsD = BE.encUint dealloc
  instr <- makeInstrM BO.RetN bitsD
  return $ Right instr

callBifM ::
     String -> A.LabelLoc -> [A.ReadLoc] -> A.CallType -> A.WriteLoc -> MResult
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
  instr <- makeInstrM op (bitsName ++ bitsFail ++ concat bitsArgs ++ bitsDst)
  return $ Right instr

deconsM :: A.ReadLoc -> A.WriteLoc -> A.WriteLoc -> MResult
deconsM src dstH dstT = do
  bitsSrc <- BE.encReadLocM src
  let bitsH = BE.encWriteLoc dstH
      bitsT = BE.encWriteLoc dstT
  instr <- makeInstrM BO.Decons (bitsSrc ++ bitsH ++ bitsT)
  return $ Right instr

consM :: A.ReadLoc -> A.ReadLoc -> A.WriteLoc -> MResult
consM h t dst = do
  bitsH <- BE.encReadLocM h
  bitsT <- BE.encReadLocM t
  let bitsDst = BE.encWriteLoc dst
  instr <- makeInstrM BO.Cons (bitsH ++ bitsT ++ bitsDst)
  return $ Right instr

selectM :: A.SelectSubj -> A.ReadLoc -> A.LabelLoc -> A.JumpTab -> MResult
selectM selType val onfail jtab = do
  let op =
        case selType of
          A.SValue      -> BO.SelectVal
          A.STupleArity -> BO.SelectTupleArity
  bitsVal <- BE.encReadLocM val
  let bitsFail = BE.encLabelLoc onfail
  bitsJt <- BE.encJtabM jtab
  instr <- makeInstrM op (bitsVal ++ bitsFail ++ bitsJt)
  return $ Right instr

jumpM :: A.LabelLoc -> MResult
jumpM lbl = do
  let bitsLbl = BE.encLabelLoc lbl
  instr <- makeInstrM BO.Jump bitsLbl
  return $ Right instr

callFunM :: Int -> MResult
callFunM arity = do
  let bitsA = BE.encUint arity
  instr <- makeInstrM BO.CallFun bitsA
  return $ Right instr

setNilM :: A.WriteLoc -> MResult
setNilM dst = do
  let bitsDst = BE.encWriteLoc dst
  instr <- makeInstrM BO.SetNil bitsDst
  return $ Right instr

trimM :: Int -> MResult
trimM n = do
  let bitsN = BE.encUint n
  instr <- makeInstrM BO.Trim bitsN
  return $ Right instr

makeFunM :: A.LabelLoc -> Int -> MResult
makeFunM lbl@(A.LabelLoc _) nfree = do
  let bitsNFree = BE.encUint nfree
      bitsLbl = BE.encLabelLoc lbl
  instr <- makeInstrM BO.MakeFun (bitsLbl ++ bitsNFree)
  return $ Right instr

bsContextToBinM :: A.ReadLoc -> MResult
bsContextToBinM src = do
  bitsSrc <- BE.encReadLocM src
  instr <- makeInstrM BO.BsContextToBin bitsSrc
  return $ Right instr

bsSaveM :: A.ReadLoc -> Int -> MResult
bsSaveM = bsSaveRestoreM BO.BsSave

bsRestoreM :: A.ReadLoc -> Int -> MResult
bsRestoreM = bsSaveRestoreM BO.BsRestore

bsSaveRestoreM :: BO.Opcode -> A.ReadLoc -> Int -> MResult
bsSaveRestoreM opcode src index = do
  bitsSrc <- BE.encReadLocM src
  let bitsI = BE.encUint index
  instr <- makeInstrM opcode (bitsSrc ++ bitsI)
  return $ Right instr

bsInitM :: Int -> Int -> A.WriteLoc -> A.LabelLoc -> MResult
bsInitM sz gcLive dst onFail = do
  let bitsSz = BE.encUint sz
      bitsLive = BE.encUint gcLive
      bitsDst = BE.encWriteLoc dst
      bitsFail = BE.encLabelLoc onFail
  instr <- makeInstrM BO.BsInit (bitsSz ++ bitsLive ++ bitsDst ++ bitsFail)
  return $ Right instr

bsPutIntegerM :: A.ReadLoc -> A.BinaryFlags -> A.WriteLoc -> MResult
bsPutIntegerM src bFlags dst = do
  bitsSrc <- BE.encReadLocM src
  let bitsBF = BE.encBinaryFlags bFlags
      bitsDst = BE.encWriteLoc dst
  instr <- makeInstrM BO.BsPutInteger (bitsSrc ++ bitsBF ++ bitsDst)
  return $ Right instr
