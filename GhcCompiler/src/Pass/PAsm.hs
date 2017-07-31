--{-# LANGUAGE InstanceSigs #-}
module Pass.PAsm
  ( transformAsmMod
  ) where

import qualified Asm                  as A
import qualified Asm.Func             as AF
import qualified Asm.Mod              as AM
import qualified Bytecode             as Bc
import           Bytecode.Func
import           Bytecode.Mod
import           Bytecode.Op
import           Term
import           Uerlc

import qualified Control.Monad.Except as MEx
import qualified Control.Monad.State  as S
import qualified Data.Map             as Map

-- Given Asm module produce Bytecode module or throw an error
transformAsmMod :: AM.Module -> BcModule
transformAsmMod amod = bcmod
  where
    bcmod0 = Bytecode.Mod.new
    funs = Map.elems $ AM.amFuns amod
    bcmod =
      case transformM funs bcmod0 `MEx.catchError` Left of
        Right bcmod' -> bcmod'
        Left e       -> Uerlc.err $ show e

-- [monadic] given list of Asm funs and a Bytecode module, update module with
-- funs that are transformed to Bytecode funs
transformM :: [AF.Func] -> BcModule -> CompileErrorOr BcModule
transformM [] bcMod = Right bcMod
transformM (fun:fTail) bcMod0 = transformM fTail bcMod1
  where
    nameArity = AF.afName fun
    bcMod1 = updateFun nameArity bcFun bcMod0
    bcFun =
      case S.evalState (transformFnM fun) bcMod0 of
        Right bcFun' -> bcFun'
        Left e       -> Uerlc.err $ show e

-- Updates/writes a func in a bytecode module, returns an updated module
updateFun :: FunArity -> BcFunc -> BcModule -> BcModule
updateFun nameArity f bcMod0 = bcMod1
  where
    funs0 = bcmFuns bcMod0
    funs1 = Map.insert nameArity f funs0
    bcMod1 = bcMod0 {bcmFuns = funs1}

-- [monadic] Given an Asm func converts it to a Bytecode func, also updates
-- the module with whatever is found on the way (atoms, literals etc)
transformFnM :: AF.Func -> S.State BcModule (CompileErrorOr BcFunc)
transformFnM fn = do
  let asmCode = AF.afCode fn
  -- bytecode <- foldM foldOpHelper [] asmCode
  trResult <- transformAsmOpsM [] asmCode
  case trResult of
    Right bytecode ->
      let outFn = BcFunc {bcfName = AF.afName fn, bcfCode = bytecode}
      in return (Right outFn)
    Left e -> return $ Left e

-- [monadic] Given an accumulator (bytecode ops) and input (a list of asm
-- opcodes) returns a list of bytecodes
transformAsmOpsM ::
     [BcOp] -> [A.UAsmOp] -> S.State BcModule (CompileErrorOr [BcOp])
transformAsmOpsM acc [] = return $ Right (reverse acc)
transformAsmOpsM acc (aop:remainingAops) = do
  trResult <- transform1M aop
  case trResult of
    Right bcop -> transformAsmOpsM (bcop ++ acc) remainingAops
    Left e     -> return $ Left e

-- [monadic] For those cases when 1:1 simple mapping between asm and bytecode
-- is enough. For complex cases add a clause in transformAsmOpsM
transform1M :: A.UAsmOp -> S.State BcModule (CompileErrorOr [BcOp])
transform1M (A.AComment _s) = return $ Right []
transform1M (A.ALabel _lb) = return $ Right []
transform1M (A.ALine _ln) = return $ Right []
transform1M (A.AError e) = return $ Right [Bc.err e]
transform1M (A.ATest tname onfail args maybeLive dst) = do
  testOp <- Bc.testM tname onfail args maybeLive dst
  return $ Right [testOp]
transform1M (A.AAlloc need live) = return $ Right [Bc.alloc need live]
transform1M (A.AMove src dst) = do
  byteCode <- Bc.moveM src dst
  return $ Right [byteCode]
transform1M (A.ACall arity codeLoc callType) = do
  byteCode <- Bc.callM arity codeLoc callType
  return $ Right [byteCode]
transform1M (A.ATestHeap needH live) = do
  let byteCode = Bc.testHeap needH live
  return $ Right [byteCode]
transform1M (A.ATupleNew sz dst) = do
  byteCode <- Bc.tupleNewM sz dst
  return $ Right [byteCode]
transform1M (A.ATuplePut val) = do
  byteCode <- Bc.tuplePutM val
  return $ Right [byteCode]
transform1M (A.ATupleGetEl src i dst) = do
  byteCode <- Bc.tupleGetElM src i dst
  return $ Right [byteCode]
transform1M (A.ATupleSetEl val index dst) = do
  byteCode <- Bc.tupleSetElM val index dst
  return $ Right [byteCode]
transform1M (A.ARet dealloc) = do
  let byteCode = Bc.ret dealloc
  return $ Right [byteCode]
transform1M (A.ACallBif name onfail args callType dst) = do
  byteCode <- Bc.callBifM name onfail args callType dst
  return $ Right [byteCode]
transform1M (A.ADecons src dstH dstT) = do
  byteCode <- Bc.deconsM src dstH dstT
  return $ Right [byteCode]
transform1M (A.ASelect selType val onfail jtab) = do
  byteCode <- Bc.selectM selType val onfail jtab
  return $ Right [byteCode]
transform1M op = return $ Uerlc.errM $ "Don't know how to compile: " ++ show op
   -- return $ Right []
