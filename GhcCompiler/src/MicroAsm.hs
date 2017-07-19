module MicroAsm where

import UModule
import UFunction
import BeamSTypes

transform :: SExpr -> Either String Module
transform (SList l) =
  let mod0 = Module {umodName = "", umodFuns = [], umodExports = []}
  in let mod1 = transform' l mod0
  in Right mod1
transform other =
  Left $ show other


isBeamSFunction :: SExpr -> Bool
isBeamSFunction (STuple (SAtom "function":_)) = True
isBeamSFunction _ = False


transform' :: [SExpr] -> Module -> Module
transform' [] mod0 = mod0

transform' (STuple [SAtom "function", fname, farity, flabel]:tl) mod0 =
  let funs0 = UModule.umodFuns mod0
      tl1 = dropWhile (not . isBeamSFunction) tl
      fbody = takeWhile (not . isBeamSFunction) tl
      outFn = parseFn fname farity flabel fbody
  in transform' tl1 mod0 {umodFuns = outFn : funs0}

transform' (STuple [SAtom "module", SAtom mname]:tl) mod0 =
  let mod1 = mod0 {umodName = mname}
  in transform' tl mod1

transform' (STuple [SAtom "exports", SList exps]:tl) mod0 =
  let exps1 = map (\(STuple [SAtom fn, SInt ar]) -> (fn, ar)) exps
      mod1 = mod0 {umodExports = exps1}
  in transform' tl mod1

transform' (STuple [SAtom "attributes", SList _mattr]:tl) mod0 =
  -- ignored at the moment
  transform' tl mod0

transform' (STuple [SAtom "labels", _]:tl) mod0 =
  transform' tl mod0

transform' (form:_tl) _mod0 =
  error ("unexpected form in the input S file: " ++ show form)


parseFn :: SExpr -> SExpr -> SExpr -> [SExpr] -> Function
parseFn (SAtom fname) (SInt farity) (SInt flabel) fbody =
  Function {ufunName = fname, ufunArity = farity, ufunBody = show fbody}
