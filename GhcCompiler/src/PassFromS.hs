{-# LANGUAGE UnicodeSyntax #-}

-- Handles input from BeamSParser and creates an Module which has separate
-- functions, and each opcode is converted to some Uassembly
module PassFromS where

import           BeamSTypes
import           UAssembly
import           UFunction
import           UModule

import qualified Data.Map   as Map

transform ∷ SExpr → Either String Module
transform (SList l) =
  let mod0 = Module {umodName = "", umodFuns = Map.empty, umodExports = []}
  in let mod1 = transform' l mod0
     in Right mod1
transform other = Left $ show other

-- Returns True if a tuple is a {function, ...} otherwise False
isBeamSFunction ∷ SExpr → Bool
isBeamSFunction (STuple (SAtom "function":_)) = True
isBeamSFunction _                             = False

-- Given list of tuples from BEAM S file handles header elements and then
-- takes functions one by one
transform' ∷ [SExpr] → Module → Module
transform' [] mod0 = mod0
transform' (STuple [SAtom "function", fname, farity, flabel]:tl) mod0 =
  let funs0 = UModule.umodFuns mod0
      tl1 = dropWhile (not . isBeamSFunction) tl
      fbody = takeWhile (not . isBeamSFunction) tl
      outFn = fnCreate fname farity flabel fbody
      funArity = sexprFunarity fname farity
      funs1 = Map.insert funArity outFn funs0
  in transform' tl1 mod0 {umodFuns = funs1}
transform' (STuple [SAtom "module", SAtom mname]:tl) mod0 =
  let mod1 = mod0 {umodName = mname}
  in transform' tl mod1
transform' (STuple [SAtom "exports", SList exps]:tl) mod0 =
  let exps1 = map (\(STuple [SAtom fn, SInt ar]) -> (fn, ar)) exps
      mod1 = mod0 {umodExports = exps1}
  in transform' tl mod1
-- ignored at the moment
transform' (STuple [SAtom "attributes", SList _mattr]:tl) mod0 =
  transform' tl mod0
transform' (STuple [SAtom "labels", _]:tl) mod0 = transform' tl mod0
transform' (form:_tl) _mod0 =
  error ("unexpected form in the input S file: " ++ show form)

-- Given F/Arity and code body return a Function object
fnCreate ∷ SExpr → SExpr → SExpr → [SExpr] → Function
fnCreate (SAtom fname) (SInt farity) (SInt _flabel) fbody =
  let asmBody = codeToAsm fbody []
  in Function {ufunName = fname, ufunArity = farity, ufunBody = asmBody}
fnCreate _f _a _label _body = error "parseFn expects a function"

codeToAsm ∷ [SExpr] → [UAsmOp] → [UAsmOp]
codeToAsm [] acc = reverse acc
codeToAsm (STuple [SAtom "label", label]:tl) acc =
  let Just nlabel = sexprInt label
      op = ULabel nlabel
  in codeToAsm tl (op : acc)
codeToAsm (STuple [SAtom "line", _]:tl) acc = codeToAsm tl acc
codeToAsm (_:tl) acc = codeToAsm tl acc
