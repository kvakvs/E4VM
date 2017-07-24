--{-# LANGUAGE InstanceSigs #-}
module TransformAsm where

import           Asm
import           Func
import           Mod
import           Bytecode
import           Func
import           Mod
import           Term
import           Uerlc

import qualified Control.Monad.Except as MEx
import qualified Control.Monad.State  as S
import qualified Data.Map             as Map

-- Given Asm module produce Bytecode module or throw an error
transformAsmMod :: AModule -> BcModule
transformAsmMod amod = bcmod
  where
    bcmod0 = Mod.new
    funs = Map.elems $ Mod.amFuns amod
    bcmod =
      case transform' funs bcmod0 `MEx.catchError` Left of
        Right bcmod' -> bcmod'
        Left e       -> Uerlc.err $ show e

-- [monadic] given list of Asm funs and a Bytecode module, update module with
-- funs that are transformed to Bytecode funs
transform' :: [AFunc] -> BcModule -> CompileErrorOr BcModule
transform' [] bcMod = Right bcMod
transform' (fun:fTail) bcMod0 = transform' fTail bcMod1
  where
    nameArity = afName fun
    bcMod1 = updateFun nameArity bcFun bcMod0
    bcFun =
      case S.evalState (transformFn fun) bcMod0 of
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
transformFn :: AFunc -> S.State BcModule (CompileErrorOr BcFunc)
transformFn fn = do
  let asmCode = afCode fn
  -- bytecode <- foldM foldOpHelper [] asmCode
  trResult <- transformAsmOps [] asmCode
  case trResult of
    Right bytecode ->
      let outFn = BcFunc {bcfName = afName fn, bcfCode = bytecode}
      in return (Right outFn)
    Left e -> return $ Left e

-- [monadic] Given an accumulator (bytecode ops) and input (a list of asm
-- opcodes) returns a list of bytecodes
transformAsmOps ::
     [BcOp] -> [UAsmOp] -> S.State BcModule (CompileErrorOr [BcOp])
transformAsmOps acc [] = return $ Right (reverse acc)
transformAsmOps acc (aop:remainingAops) = do
  trResult <- transform1Op aop
  case trResult of
    Right bcop -> transformAsmOps (bcop ++ acc) remainingAops
    Left e     -> return $ Left e

-- For those cases when 1:1 simple mapping between asm and bytecode is enough
transform1Op :: UAsmOp -> S.State BcModule (CompileErrorOr [BcOp])
transform1Op (Asm.AComment _s) = return $ Right []
transform1Op (Asm.ALabel _lb) = return $ Right []
transform1Op (Asm.ALine _ln) = return $ Right []
transform1Op (Asm.AError e) = return $ Right [Bytecode.err e]
transform1Op (Asm.ATest tname onfail args maybeLive dst) = return $ Right [Bytecode.test tname]
transform1Op op = return $ Uerlc.errM $ "Don't know how to compile: " ++ show op
