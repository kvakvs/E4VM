-- Handles input from BeamSParser and creates an Module which has separate
-- functions, and each opcode is converted to some Asm
module PassFromS where

import           BeamSTypes
import           Asm
import           Uerlc
import           AsmFunc
import           AsmMod

import           Control.Exception
import qualified Data.Map          as Map
import           Data.Maybe        (fromJust)

transform :: SExpr -> Either String Module
transform (SList l) =
  let mod0 = Module {umodName = "", umodFuns = Map.empty, umodExports = []}
  in let mod1 = transform' l mod0
     in Right mod1
transform other = Left $ show other

-- Returns True if a tuple is a {function, ...} otherwise False
isBeamSFunction :: SExpr -> Bool
isBeamSFunction (STuple (SAtom "function":_)) = True
isBeamSFunction _                             = False

-- Given list of tuples from BEAM S file handles header elements and then
-- takes functions one by one
transform' :: [SExpr] -> Module -> Module
transform' [] mod0 = mod0
transform' (STuple [SAtom "function", fname, farity, flabel]:tl) mod0 =
  let funs0 = AsmMod.umodFuns mod0
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
  Uerlc.err ("unexpected form in the input S file: " ++ show form)

-- Given F/Arity and code body return a Function object
fnCreate :: SExpr -> SExpr -> SExpr -> [SExpr] -> Function
fnCreate (SAtom fname) (SInt farity) (SInt _flabel) fbody =
  let asmBody = transformCode fbody []
  in Function {ufunName = fname, ufunArity = farity, ufunBody = asmBody}
fnCreate _f _a _label _body = Uerlc.err "parseFn expects a function"

-- Given a beamS expression, parse an Asm data source
readLoc :: SExpr -> Maybe ReadLoc
readLoc (STuple [SAtom "x", SInt x])       = Just $ RRegX (fromIntegral x)
readLoc (STuple [SAtom "y", SInt y])       = Just $ RRegY (fromIntegral y)
readLoc (STuple [SAtom "literal", lit])    = Just $ RLit lit
readLoc (STuple [SAtom "atom", a])         = Just $ RAtom a
readLoc (SAtom "nil")                      = Just RNil
readLoc (SInt i)                           = Just $ RInt i
readLoc (STuple [SAtom "integer", SInt i]) = Just $ RInt i
readLoc other                              = Just $ ReadLocError $ show other

-- Given a beamS expression parse an Asm data destination
writeLoc :: SExpr -> Maybe WriteLoc
writeLoc (STuple [SAtom "x", SInt x]) = Just $ WRegX (fromIntegral x)
writeLoc (STuple [SAtom "y", SInt y]) = Just $ WRegY (fromIntegral y)
writeLoc other                        = Just $ WriteLocError $ show other

parseLabel :: SExpr -> LabelLoc
parseLabel (STuple [SAtom "f", SInt 0]) = UNoLabel
parseLabel (STuple [SAtom "f", SInt i]) = LabelLoc $ fromIntegral i
parseLabel other = Uerlc.err ("not a label" ++ show other)

parseChoices :: [SExpr] -> [(SExpr, LabelLoc)] -> [(SExpr, LabelLoc)]
parseChoices [] acc = reverse acc
parseChoices [_] _acc = Uerlc.err "parseChoices given a list of odd length"
parseChoices (val:lbl:tl) acc = parseChoices tl acc1
  where
    ulbl = parseLabel lbl
    acc1 = (val, ulbl) : acc

transformCode :: [SExpr] -> [UAsmOp] -> [UAsmOp]
transformCode [] acc = reverse acc
transformCode (STuple [SAtom "label", f]:tl) acc = transformCode tl (op : acc)
  where
    Just nlabel = sexprInt f
    op = Asm.label nlabel
transformCode (STuple [SAtom "line", _]:tl) acc = transformCode tl acc
transformCode (STuple [SAtom "move", src, dst]:tl) acc =
  transformCode tl (Asm.move usrc udst : acc)
  where
    Just usrc = readLoc src
    Just udst = writeLoc dst
transformCode (STuple [SAtom "get_list", src, hddst, tldst]:tl) acc =
  transformCode tl (op : acc)
  where
    Just usrc = readLoc src
    Just uhd = writeLoc hddst
    Just utl = writeLoc tldst
    op = Asm.decons usrc uhd utl
transformCode (STuple [SAtom "put_list", h, t, dst]:tl) acc =
  transformCode tl (op : acc)
  where
    Just uhead = readLoc h
    Just utail = readLoc t
    Just udst = writeLoc dst
    op = Asm.cons uhead utail udst
transformCode (STuple [SAtom "func_info", _mod, _fun, _arity]:tl) acc =
  transformCode tl (Asm.funcClause : acc)
transformCode (STuple [SAtom "case_end", _dst]:tl) acc =
  transformCode tl (Asm.caseClause : acc)
transformCode (STuple [SAtom "if_end", _dst]:tl) acc =
  transformCode tl (Asm.ifClause : acc)
transformCode (STuple [SAtom "badmatch", val]:tl) acc =
  transformCode tl (op : acc)
  where
    Just uval = readLoc val
    op = Asm.badmatch uval
transformCode (STuple [SAtom "put_tuple", sz, dst]:tl) acc =
  transformCode tl (Asm.tupleNew usz udst : acc)
  where
    Just udst = writeLoc dst
    Just usz = sexprInt sz
transformCode (STuple [SAtom "put", val]:tl) acc =
  transformCode tl (Asm.tuplePut uval : acc)
  where
    Just uval = readLoc val
transformCode (STuple [SAtom "get_tuple_element", src, indx, dst]:tl) acc =
  transformCode tl (Asm.tupleGetEl usrc uindx udst : acc)
  where
    Just usrc = readLoc src
    Just uindx = readLoc indx
    Just udst = writeLoc dst
transformCode (STuple [SAtom "set_tuple_element", dst, tup, indx]:tl) acc =
  transformCode tl (op : acc)
  where
    Just utup = readLoc tup
    Just udst = writeLoc dst
    Just uindx = readLoc indx
    op = Asm.tupleSetEl utup uindx udst
transformCode (STuple [SAtom "jump", dst]:tl) acc = transformCode tl (op : acc)
  where
    udst = parseLabel dst
    op = Asm.jump udst
transformCode (STuple [SAtom opname, stkneed, live]:tl) acc
  | opname == "allocate" || opname == "allocate_zero" =
    transformCode tl (op : acc)
  where
    Just ustkneed = sexprInt stkneed
    Just ulive = sexprInt live
    op = Asm.allocate ustkneed ulive
transformCode (STuple [SAtom "deallocate", n]:SAtom "return":tl) acc =
  transformCode tl (Asm.ret un : acc)
  where
    Just un = sexprInt n
transformCode (STuple [SAtom "deallocate", n]:tl) acc =
  transformCode tl (Asm.deallocate un : acc)
  where
    Just un = sexprInt n
transformCode (SAtom "return":tl) acc = transformCode tl (op : acc)
  where
    op = Asm.ret 0
-- {test,Cond,Fail,Ops}
transformCode (STuple [SAtom "test", SAtom testName, fail1, SList args]:tl) acc =
  transformCode tl (op : acc)
  where
    ufail = parseLabel fail1
    uargs = map (fromJust . readLoc) args
    op = Asm.test testName ufail uargs Nothing WIgnore
-- {test,Cond,Fail,Live,Ops,Dst}
transformCode (STuple [SAtom "test", SAtom testName, fail1, live, SList args, dst]:tl) acc =
  transformCode tl (op : acc)
  where
    ufail = parseLabel fail1
    uargs = map (fromJust . readLoc) args
    Just udst = writeLoc dst
    Just ulive = sexprInt live
    op = Asm.test testName ufail uargs (Just ulive) udst
-- {test,Cond,Fail,Src,Ops}
-- TODO
transformCode (STuple (SAtom callOp:_arity:mfa:deallc):tl) acc
  | callOp == "call_ext" ||
      callOp == "call_ext_only" || callOp == "call_ext_last" =
    transformCode tl (op : acc)
  where
    STuple [SAtom "extfunc", SAtom m, SAtom f, arity] = mfa
    Just uarity = sexprInt arity
    Just udeallc = sexprInt $ head deallc
    callType =
      case callOp of
        "call_ext"      -> NormalCall
        "call_ext_only" -> TailCall
        "call_ext_last" -> TailCallDealloc udeallc
        _               -> Uerlc.err "Bad call op type"
    op = Asm.callExt (m, f, uarity) callType
transformCode (STuple (SAtom callOp:arity:dst:deallc):tl) acc
  | callOp == "call" || callOp == "call_only" || callOp == "call_last" =
    transformCode tl (op : acc)
  where
    udst = parseLabel dst
    Just uarity = sexprInt arity
    [deallc0] = deallc
    Just udeallc = sexprInt deallc0
    callType =
      case callOp of
        "call"      -> NormalCall
        "call_only" -> TailCall
        "call_last" -> TailCallDealloc udeallc
        _           -> Uerlc.err "Bad call op type"
    op = Asm.callLabel uarity udst callType
transformCode (STuple [SAtom "call_fun", arity]:tl) acc =
  transformCode tl (op : acc)
  where
    Just uarity = sexprInt arity
    op = Asm.callFun uarity
transformCode (STuple [SAtom killOp, dst]:tl) acc
  | killOp == "kill" || killOp == "init" = transformCode tl (op : acc)
  where
    Just udst = writeLoc dst
    op = Asm.setNil udst
transformCode (STuple [SAtom "gc_bif", SAtom bifName, onfail, live, SList args, dst]:tl) acc =
  transformCode tl (op : acc)
  where
    ufail = parseLabel onfail
    Just udst = writeLoc dst
    uargs = map (fromJust . readLoc) args
    Just ulive = sexprInt live
    op = Asm.callBif bifName ufail uargs (GcEnabledCall ulive) udst
transformCode (STuple [SAtom "bif", SAtom bifName, onfail, SList args, dst]:tl) acc =
  transformCode tl (op : acc)
  where
    ufail = parseLabel onfail
    Just udst = writeLoc dst
    uargs = map (fromJust . readLoc) args
    op = Asm.callBif bifName ufail uargs NormalCall udst
transformCode (STuple [SAtom "test_heap", need, live]:tl) acc =
  transformCode tl (op : acc)
  where
    Just uneed = sexprInt need
    Just ulive = sexprInt live
    op = Asm.testHeap uneed ulive
transformCode (STuple [SAtom "trim", n, _remaining]:tl) acc =
  transformCode tl (op : acc)
  where
    Just un = sexprInt n
    op = Asm.trim un
transformCode (STuple [SAtom selOp, src, onfail, STuple [SAtom "list", SList choices]]:tl) acc
  | selOp == "select_val" || selOp == "select_tuple_arity" =
    transformCode tl (op : acc)
  where
    Just usrc = readLoc src
    ufail = parseLabel onfail
    uchoices = parseChoices choices []
    op =
      case selOp of
        "select_val" -> Asm.select SelectVal usrc ufail uchoices
        "select_tuple_arity" ->
          Asm.select SelectTupleArity usrc ufail uchoices
transformCode (STuple [SAtom "make_fun2", lbl, _indx, _olduniq, numfree]:tl) acc =
  transformCode tl (op : acc)
  where
    ulbl = parseLabel lbl
    Just unumfree = sexprInt numfree
    op = Asm.makeFun ulbl unumfree
transformCode (STuple [SAtom "bs_context_to_binary", src]:tl) acc =
  transformCode tl (op : acc)
  where
    Just usrc = readLoc src
    op = Asm.bsContextToBin usrc
transformCode (STuple [SAtom "bs_init2", onfail, sz, _extra, live, _flags, dst]:tl) acc =
  transformCode tl (op : acc)
  where
    Just usz = sexprInt sz
    Just ulive = sexprInt live
    Just udst = writeLoc dst
    uonfail = parseLabel onfail
    op = Asm.bsInit usz ulive udst uonfail
transformCode (STuple [SAtom bsOp, src, indx]:tl) acc
  | bsOp == "bs_save2" || bsOp == "bs_restore2" = transformCode tl (op : acc)
  where
    Just usrc = readLoc src
    Just uindx = sexprInt indx
    op =
      case bsOp of
        "bs_save2"    -> Asm.bsSave usrc uindx
        "bs_restore2" -> Asm.bsRestore usrc uindx
        other ->
          Uerlc.err $ "can't create bs_ command for " ++ show other
transformCode (STuple [SAtom "bs_put_integer", _onfail, src, _n, flags, dst]:tl) acc =
  transformCode tl (op : acc)
  where
    Just usrc = readLoc src
    Just udst = writeLoc dst
    uflags = parseBinaryFlags flags
    op = Asm.bsPutInteger usrc uflags udst
transformCode (STuple (SAtom "%":_):tl) acc = transformCode tl acc
transformCode (other:_tl) _acc =
  Uerlc.err ("don't know how to transform " ++ show other)

parseBinaryFlags :: SExpr -> BinaryFlags
parseBinaryFlags (STuple [SAtom "field_flags", SList flgs]) =
  BinaryFlags unit sig big
  where
    unit = 8
    sig = SAtom "signed" `elem` flgs
    big = SAtom "big" `elem` flgs
parseBinaryFlags other =
  Uerlc.err $ "can't parse binary flags from " ++ show other
