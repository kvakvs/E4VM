--{-# LANGUAGE InstanceSigs #-}
module Pass.PAsm
  ( transformAsmMod
  ) where

import qualified Asm
import           Asm.Func
import           Asm.Mod
import qualified Bytecode
import           Bytecode.Func
import           Bytecode.Mod
import           Bytecode.Op
import           Term
import           Uerlc

import qualified Control.Monad.Except as MEx
import qualified Control.Monad.State  as S
import qualified Data.Map             as Map

-- Given Asm module produce Bytecode module or throw an error
transformAsmMod :: AModule -> BcModule
transformAsmMod amod = bcmod
  where
    bcmod0 = Bytecode.Mod.new
    funs = Map.elems $ Asm.Mod.amFuns amod
    bcmod =
      case transformM funs bcmod0 `MEx.catchError` Left of
        Right bcmod' -> bcmod'
        Left e       -> Uerlc.err $ show e

-- [monadic] given list of Asm funs and a Bytecode module, update module with
-- funs that are transformed to Bytecode funs
transformM :: [AFunc] -> BcModule -> CompileErrorOr BcModule
transformM [] bcMod = Right bcMod
transformM (fun:fTail) bcMod0 = transformM fTail bcMod1
  where
    nameArity = afName fun
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
transformFnM :: AFunc -> S.State BcModule (CompileErrorOr BcFunc)
transformFnM fn = do
  let asmCode = afCode fn
  -- bytecode <- foldM foldOpHelper [] asmCode
  trResult <- transformAsmOpsM [] asmCode
  case trResult of
    Right bytecode ->
      let outFn = BcFunc {bcfName = afName fn, bcfCode = bytecode}
      in return (Right outFn)
    Left e -> return $ Left e

-- [monadic] Given an accumulator (bytecode ops) and input (a list of asm
-- opcodes) returns a list of bytecodes
transformAsmOpsM ::
     [BcOp] -> [Asm.UAsmOp] -> S.State BcModule (CompileErrorOr [BcOp])
transformAsmOpsM acc [] = return $ Right (reverse acc)
transformAsmOpsM acc (aop:remainingAops) = do
  trResult <- transform1M aop
  case trResult of
    Right bcop -> transformAsmOpsM (bcop ++ acc) remainingAops
    Left e     -> return $ Left e

-- [monadic] For those cases when 1:1 simple mapping between asm and bytecode
-- is enough. For complex cases add a clause in transformAsmOpsM
transform1M :: Asm.UAsmOp -> S.State BcModule (CompileErrorOr [BcOp])
transform1M (Asm.AComment _s) = return $ Right []
transform1M (Asm.ALabel _lb) = return $ Right []
transform1M (Asm.ALine _ln) = return $ Right []
transform1M (Asm.AError e) = return $ Right [Bytecode.err e]
transform1M (Asm.ATest tname onfail args maybeLive dst) = do
  testOp <- Bytecode.testM tname onfail args maybeLive dst
  return $ Right [testOp]
transform1M (Asm.AAlloc need live) = return $ Right [Bytecode.alloc need live]
transform1M (Asm.AMove src dst) = do
  byteCode <- Bytecode.moveM src dst
  return $ Right [byteCode]
transform1M (Asm.ACall arity codeLoc callType) = do
  byteCode <- Bytecode.callM arity codeLoc callType
  return $ Right [byteCode]
transform1M (Asm.ATestHeap needH live) = do
  let byteCode = Bytecode.testHeap needH live
  return $ Right [byteCode]
transform1M (Asm.ATupleNew sz dst) = do
  byteCode <- Bytecode.tupleNewM sz dst
  return $ Right [byteCode]
transform1M (Asm.ATuplePut val) = do
  byteCode <- Bytecode.tuplePutM val
  return $ Right [byteCode]
transform1M (Asm.ATupleGetEl src i dst) = do
  byteCode <- Bytecode.tupleGetElM src i dst
  return $ Right [byteCode]
transform1M (Asm.ATupleSetEl val index dst) = do
  byteCode <- Bytecode.tupleSetElM val index dst
  return $ Right [byteCode]
transform1M (Asm.ARet dealloc) = do
  let byteCode = Bytecode.ret dealloc
  return $ Right [byteCode]
transform1M op = return $ Uerlc.errM $ "Don't know how to compile: " ++ show op
   -- return $ Right []
