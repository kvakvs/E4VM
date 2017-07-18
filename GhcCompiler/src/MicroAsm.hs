module MicroAsm where

import BeamSTypes
import UModule
import UFunction


transform :: BeamSExpr -> Either String Module
transform (BeamSList l) =
  let mod0 = Module {funs = []}
  in let mod1 = transform' l mod0
  in Right mod1
transform other =
  Left $ show other

transform' :: [BeamSExpr] -> Module -> Module
transform' [] mod0 = mod0
transform' (h:tl) mod0 =
  let outFn = transformFn h
      funs0 = UModule.funs mod0
  in transform' tl mod0 {funs = outFn:funs0}


transformFn :: BeamSExpr -> Function
transformFn _ =
  Function {name = "test", arity = 0}
