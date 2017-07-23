--{-# LANGUAGE InstanceSigs #-}
module TransformAsm where

import           Asm
import           AsmFunc
import           AsmMod
import           Bytecode
import           BytecodeFunc
import           BytecodeMod
import           Term

import qualified Control.Monad.State as S
import qualified Data.Map            as Map

transformAsmMod :: AModule -> BcModule
transformAsmMod amod = bcmod
  where
    bcmod0 = BytecodeMod.new
    funs = Map.elems $ AsmMod.amFuns amod
    bcmod = transform' funs bcmod0

transform' [] bcMod = bcMod
transform' (fun:fTail) bcMod0 = transform' fTail bcMod1
  where
    bcFun = S.evalState (transformFn fun) bcMod0
    nameArity = afName fun
    bcMod1 = updateFun nameArity bcFun bcMod0

updateFun :: FunArity -> BcFunc -> BcModule -> BcModule
updateFun nameArity f bcMod0 = bcMod1
  where
    funs0 = bcmFuns bcMod0
    funs1 = Map.insert nameArity f funs0
    bcMod1 = bcMod0 {bcmFuns = funs1}

--foldM _foldFn accum [] = return $ reverse accum
--foldM foldFn accum (x:xs) = do
--  accum1 <- foldFn accum x
--  foldM foldFn accum1 xs
--
--foldOpHelper :: [BcOp] -> UAsmOp  -> S.State BcModule [BcOp]
--foldOpHelper acc aop = do
--  bcop <- compileOp aop
--  return $ bcop ++ acc

-- Monadic transform carrying bytecodeModule as state, and applying transform
transformFn :: AFunc -> S.State BcModule BcFunc
transformFn fn = do
  st0 <- S.get
  let asmCode = afCode fn
  -- bytecode <- foldM foldOpHelper [] asmCode
  bytecode <- transformAsmOps [] asmCode
  return $ BcFunc (afName fn) bytecode

--compileOp aop = do
--  return [BcOp 0 []]

transformAsmOps :: [BcOp] -> [UAsmOp] -> S.State BcModule [BcOp]
transformAsmOps acc (aop:aops) = do
  bcop <- transformOneOp aop
  return $ bcop ++ acc

-- For those cases when 1:1 simple mapping between asm and bytecode exists
transformOneOp :: UAsmOp -> S.State BcModule [BcOp]
transformOneOp op = return []
