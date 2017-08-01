module Pass.PassAsm
  ( transform
  ) where

import qualified Asm                  as A
import qualified Asm.Func             as AF
import qualified Asm.Mod              as AM
import qualified Bytecode             as B
import qualified Bytecode.Func        as BF
import qualified Bytecode.Mod         as BM
import qualified Bytecode.Op          as BO
import qualified Term                 as T
import           Uerlc

import qualified Control.Monad.Except as MEx
import qualified Control.Monad.State  as S
import qualified Data.Map             as Map

-- Given Asm module produce Bytecode module or throw an error
transform :: AM.Module -> BM.Module
transform amod = bcmod
  where
    bcmod0 = BM.new
    funs = Map.elems $ AM.funs amod
    bcmod =
      case transformM funs bcmod0 `MEx.catchError` Left of
        Right bcmod' -> bcmod'
        Left e       -> Uerlc.err $ show e

-- [monadic] given list of Asm funs and a Bytecode module, update module with
-- funs that are transformed to Bytecode funs
transformM :: [AF.Func] -> BM.Module -> CompileErrorOr BM.Module
transformM [] bcMod = Right bcMod
transformM (fun:fTail) bcMod0 = transformM fTail bcMod1
  where
    nameArity = AF.name fun
    bcMod1 = updateFun nameArity bcFun bcMod0
    bcFun =
      case S.evalState (transformFnM fun) bcMod0 of
        Right bcFun' -> bcFun'
        Left e       -> Uerlc.err $ show e

-- Updates/writes a func in a bytecode module, returns an updated module
updateFun :: T.FunArity -> BF.Func -> BM.Module -> BM.Module
updateFun nameArity f bcMod0 = bcMod1
  where
    funs0 = BM.funs bcMod0
    funs1 = Map.insert nameArity f funs0
    bcMod1 = bcMod0 {BM.funs = funs1}

-- [monadic] Given an Asm func converts it to a Bytecode func, also updates
-- the module with whatever is found on the way (atoms, literals etc)
transformFnM :: AF.Func -> BM.ModuleState (CompileErrorOr BF.Func)
transformFnM fn = do
  let asmCode = AF.code fn
  -- bytecode <- foldM foldOpHelper [] asmCode
  trResult <- transformAsmOpsM [] asmCode
  case trResult of
    Right bytecode ->
      let outFn = BF.Func {BF.bcfName = AF.name fn, BF.bcfCode = bytecode}
      in return (Right outFn)
    Left e -> return $ Left e

-- [monadic] Given an accumulator (bytecode ops) and input (a list of asm
-- opcodes) returns a list of bytecodes
transformAsmOpsM ::
     [BO.Instruction]
  -> [A.Instruction]
  -> BM.ModuleState (CompileErrorOr [BO.Instruction])
transformAsmOpsM acc [] = return $ Right (reverse acc)
transformAsmOpsM acc (aop:remainingAops) = do
  trResult <- transform1M aop
  case trResult of
    Right bcop -> transformAsmOpsM (bcop ++ acc) remainingAops
    Left e     -> return $ Left e

-- [monadic] For those cases when 1:1 simple mapping between asm and bytecode
-- is enough. For complex cases add a clause in transformAsmOpsM
transform1M ::
     A.Instruction -> BM.ModuleState (CompileErrorOr [BO.Instruction])
transform1M (A.AComment _s) = return $ Right []
transform1M (A.ALabel _lb) = return $ Right []
transform1M (A.ALine _ln) = return $ Right []
transform1M (A.AError e) = return $ Right [B.err e]
transform1M (A.ATest tname onfail args maybeLive dst) = do
  testOp <- B.testM tname onfail args maybeLive dst
  return $ Right [testOp]
transform1M (A.AAlloc need live) = return $ Right [B.alloc need live]
transform1M (A.AMove src dst) = do
  byteCode <- B.moveM src dst
  return $ Right [byteCode]
transform1M (A.ACall arity codeLoc callType) = do
  byteCode <- B.callM arity codeLoc callType
  return $ Right [byteCode]
transform1M (A.ATestHeap needH live) = do
  let byteCode = B.testHeap needH live
  return $ Right [byteCode]
transform1M (A.ATupleNew sz dst) = do
  byteCode <- B.tupleNewM sz dst
  return $ Right [byteCode]
transform1M (A.ATuplePut val) = do
  byteCode <- B.tuplePutM val
  return $ Right [byteCode]
transform1M (A.ATupleGetEl src i dst) = do
  byteCode <- B.tupleGetElM src i dst
  return $ Right [byteCode]
transform1M (A.ATupleSetEl val index dst) = do
  byteCode <- B.tupleSetElM val index dst
  return $ Right [byteCode]
transform1M (A.ARet dealloc) = do
  let byteCode = B.ret dealloc
  return $ Right [byteCode]
transform1M (A.ACallBif name onfail args callType dst) = do
  byteCode <- B.callBifM name onfail args callType dst
  return $ Right [byteCode]
transform1M (A.ADecons src dstH dstT) = do
  byteCode <- B.deconsM src dstH dstT
  return $ Right [byteCode]
transform1M (A.ACons h t dst) = do
  byteCode <- B.consM h t dst
  return $ Right [byteCode]
transform1M (A.ASelect selType val onfail jtab) = do
  byteCode <- B.selectM selType val onfail jtab
  return $ Right [byteCode]
transform1M (A.AJump lbl) = do
  let byteCode = B.jump lbl
  return $ Right [byteCode]
transform1M (A.ACallFun arity) = do
  let byteCode = B.callFun arity
  return $ Right [byteCode]
transform1M (A.ASetNil dst) = do
  let byteCode = B.setNil dst
  return $ Right [byteCode]
transform1M op = return $ Uerlc.errM $ "Don't know how to compile: " ++ show op
   -- return $ Right []
