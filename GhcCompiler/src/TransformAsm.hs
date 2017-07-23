module TransformAsm where

import           AsmFunc
import           AsmMod
import           BytecodeFunc
import           BytecodeMod
import           Term

import qualified Data.Map     as Map

transformAsmMod :: AModule -> BcModule
transformAsmMod amod = bcmod
  where
    bcmod0 = BytecodeMod.new
    funs = Map.elems $ AsmMod.amFuns amod
    bcmod = transform' funs bcmod0

transform' [] bcMod = bcMod
transform' (fun:fTail) bcMod0 = transform' fTail bcMod1
  where
    bcFun = transformFn fun bcMod0
    nameArity = afName fun
    bcMod1 = updateFun nameArity bcFun bcMod0

updateFun :: FunArity -> BcFunc -> BcModule -> BcModule
updateFun nameArity f bcMod0 = bcMod1
  where
    funs0 = bcmFuns bcMod0
    funs1 = Map.insert nameArity f funs0
    bcMod1 = bcMod0 {bcmFuns = funs1}

transformFn :: AFunc -> BcModule -> BcFunc
transformFn fn bcMod0 = bcFun
  where
    bcFun = BcFunc (afName fn) []
