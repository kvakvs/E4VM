--{-# LANGUAGE InstanceSigs #-}
module TransformAsm where

import           Asm
import           AsmFunc
import           AsmMod
import           Bytecode
import           BytecodeFunc
import           BytecodeMod
import           Term
import           Uerlc

import qualified Control.Monad.Except as MEx
import qualified Control.Monad.State  as S
import qualified Data.Map             as Map

transformAsmMod :: AModule -> BcModule
transformAsmMod amod = bcmod
  where
    bcmod0 = BytecodeMod.new
    funs = Map.elems $ AsmMod.amFuns amod
    bcmod =
      case transform' funs bcmod0 `MEx.catchError` Left of
        Right bcmod' -> bcmod'
        Left e       -> Uerlc.err $ show e

transform' :: [AFunc] -> BcModule -> CompileErrorOr BcModule
transform' [] bcMod = Right bcMod
transform' (fun:fTail) bcMod0 = transform' fTail bcMod1
  where
    nameArity = afName fun
    bcMod1 = updateFun nameArity bcFun bcMod0
    bcFun = case S.evalState (transformFn fun) bcMod0 of
      Right bcFun' -> bcFun'
      Left e -> Uerlc.err $ show e

updateFun :: FunArity -> BcFunc -> BcModule -> BcModule
updateFun nameArity f bcMod0 = bcMod1
  where
    funs0 = bcmFuns bcMod0
    funs1 = Map.insert nameArity f funs0
    bcMod1 = bcMod0 {bcmFuns = funs1}

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

transformAsmOps ::
     [BcOp] -> [UAsmOp] -> S.State BcModule (CompileErrorOr [BcOp])
transformAsmOps acc [] = return $ Right (reverse acc)
transformAsmOps acc (aop:remainingAops) = do
  trResult <- transformOneOp aop
  case trResult of
    Right bcop -> transformAsmOps (bcop ++ acc) remainingAops
    Left e     -> return $ Left e

-- For those cases when 1:1 simple mapping between asm and bytecode exists
transformOneOp :: UAsmOp -> S.State BcModule (CompileErrorOr [BcOp])
transformOneOp op =
  return $ Uerlc.errM $ "Don't know how to compile " ++ show op
