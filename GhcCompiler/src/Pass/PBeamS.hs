-- Handles input from BeamSParser and creates an Module which has separate
-- functions, and each opcode is converted to some Asm
module Pass.PBeamS where

import           Asm
import           Asm.Func
import           Asm.Mod
import           Term
import           Uerlc

import qualified Data.Map   as Map
import           Data.Maybe (fromJust)

transform :: Term -> AModule
transform (ErlList l) = mod1
  where
    mod0 = AModule {amName = "", amFuns = Map.empty, amExports = []}
    mod1 = transform' l mod0
transform other = Uerlc.err $ show other

-- Returns True if a tuple is a {function, ...} otherwise False
isBeamSFunction :: Term -> Bool
isBeamSFunction (ErlTuple (Atom "function":_)) = True
isBeamSFunction _                              = False

-- Given list of tuples from BEAM S file handles header elements and then
-- takes functions one by one
transform' :: [Term] -> AModule -> AModule
transform' [] mod0 = mod0
transform' (ErlTuple [Atom "function", fname, farity, flabel]:tl) mod0 =
  transform' tl1 mod0 {amFuns = funs1}
  where
    funs0 = Asm.Mod.amFuns mod0
    tl1 = dropWhile (not . isBeamSFunction) tl
    fbody = takeWhile (not . isBeamSFunction) tl
    outFn = fnCreate fname farity flabel fbody
    funArity = funarityFromErl fname farity
    funs1 = Map.insert funArity outFn funs0
transform' (ErlTuple [Atom "module", Atom mname]:tl) mod0 = transform' tl mod1
  where
    mod1 = mod0 {amName = mname}
transform' (ErlTuple [Atom "exports", ErlList exps]:tl) mod0 =
  transform' tl mod1
  where
    exps1 = map (\(ErlTuple [fn, ar]) -> funarityFromErl fn ar) exps
    mod1 = mod0 {amExports = exps1}
-- ignored at the moment
transform' (ErlTuple [Atom "attributes", ErlList _mattr]:tl) mod0 =
  transform' tl mod0
transform' (ErlTuple [Atom "labels", _]:tl) mod0 = transform' tl mod0
transform' (form:_tl) _mod0 =
  Uerlc.err ("unexpected form in the input S file: " ++ show form)

-- Given F/Arity and code body return a Function object
fnCreate :: Term -> Term -> Term -> [Term] -> AFunc
fnCreate (Atom fname) farity0 (ErlInt _flabel) fbody =
  AFunc {afName = FunArity fname farity, afCode = asmBody}
  where
    Just farity = intFromErl farity0
    asmBody = transformCode fbody []
fnCreate _f _a _label _body = Uerlc.err "parseFn expects a function"

-- Given a beamS expression, parse an Asm data source
readLoc :: Term -> Maybe ReadLoc
readLoc (ErlTuple [Atom "x", ErlInt x]) = Just $ RRegX (fromIntegral x)
readLoc (ErlTuple [Atom "y", ErlInt y]) = Just $ RRegY (fromIntegral y)
readLoc (ErlTuple [Atom "literal", lit]) = Just $ RLit lit
readLoc (ErlTuple [Atom "atom", a]) = Just $ RAtom a
readLoc (Atom "nil") = Just RNil
readLoc (ErlInt i) = Just $ RInt i
readLoc (ErlTuple [Atom "integer", ErlInt i]) = Just $ RInt i
readLoc other = Just $ ReadLocError $ show other

-- Given a beamS expression parse an Asm data destination
writeLoc :: Term -> Maybe WriteLoc
writeLoc (ErlTuple [Atom "x", ErlInt x]) = Just $ WRegX (fromIntegral x)
writeLoc (ErlTuple [Atom "y", ErlInt y]) = Just $ WRegY (fromIntegral y)
writeLoc other                           = Just $ WriteLocError $ show other

parseLabel :: Term -> LabelLoc
parseLabel (ErlTuple [Atom "f", ErlInt 0]) = UNoLabel
parseLabel (ErlTuple [Atom "f", ErlInt i]) = LabelLoc $ fromIntegral i
parseLabel other = Uerlc.err ("not a label" ++ show other)

parseChoices :: [Term] -> [(Term, LabelLoc)] -> [(Term, LabelLoc)]
parseChoices [] acc = reverse acc
parseChoices [_] _acc = Uerlc.err "parseChoices given a list of odd length"
parseChoices (val:lbl:tl) acc = parseChoices tl acc1
  where
    ulbl = parseLabel lbl
    acc1 = (val, ulbl) : acc

transformCode :: [Term] -> [UAsmOp] -> [UAsmOp]
transformCode [] acc = reverse acc
transformCode (ErlTuple [Atom "label", f]:tl) acc = transformCode tl (op : acc)
  where
    Just nlabel = intFromErl f
    op = Asm.label nlabel
transformCode (ErlTuple [Atom "line", _]:tl) acc = transformCode tl acc
transformCode (ErlTuple [Atom "move", src, dst]:tl) acc =
  transformCode tl (Asm.move usrc udst : acc)
  where
    Just usrc = readLoc src
    Just udst = writeLoc dst
transformCode (ErlTuple [Atom "get_list", src, hddst, tldst]:tl) acc =
  transformCode tl (op : acc)
  where
    Just usrc = readLoc src
    Just uhd = writeLoc hddst
    Just utl = writeLoc tldst
    op = Asm.decons usrc uhd utl
transformCode (ErlTuple [Atom "put_list", h, t, dst]:tl) acc =
  transformCode tl (op : acc)
  where
    Just uhead = readLoc h
    Just utail = readLoc t
    Just udst = writeLoc dst
    op = Asm.cons uhead utail udst
transformCode (ErlTuple [Atom "func_info", _mod, _fun, _arity]:tl) acc =
  transformCode tl (Asm.funcClause : acc)
transformCode (ErlTuple [Atom "case_end", _dst]:tl) acc =
  transformCode tl (Asm.caseClause : acc)
transformCode (ErlTuple [Atom "if_end", _dst]:tl) acc =
  transformCode tl (Asm.ifClause : acc)
transformCode (ErlTuple [Atom "badmatch", val]:tl) acc =
  transformCode tl (op : acc)
  where
    Just uval = readLoc val
    op = Asm.badmatch uval
transformCode (ErlTuple [Atom "put_tuple", sz, dst]:tl) acc =
  transformCode tl (Asm.tupleNew usz udst : acc)
  where
    Just udst = writeLoc dst
    Just usz = intFromErl sz
transformCode (ErlTuple [Atom "put", val]:tl) acc =
  transformCode tl (Asm.tuplePut uval : acc)
  where
    Just uval = readLoc val
transformCode (ErlTuple [Atom "get_tuple_element", src, indx, dst]:tl) acc =
  transformCode tl (Asm.tupleGetEl usrc uindx udst : acc)
  where
    Just usrc = readLoc src
    Just uindx = readLoc indx
    Just udst = writeLoc dst
transformCode (ErlTuple [Atom "set_tuple_element", dst, tup, indx]:tl) acc =
  transformCode tl (op : acc)
  where
    Just utup = readLoc tup
    Just udst = writeLoc dst
    Just uindx = readLoc indx
    op = Asm.tupleSetEl utup uindx udst
transformCode (ErlTuple [Atom "jump", dst]:tl) acc = transformCode tl (op : acc)
  where
    udst = parseLabel dst
    op = Asm.jump udst
transformCode (ErlTuple [Atom opname, stkneed, live]:tl) acc
  | opname == "allocate" || opname == "allocate_zero" =
    transformCode tl (op : acc)
  where
    Just ustkneed = intFromErl stkneed
    Just ulive = intFromErl live
    op = Asm.allocate ustkneed ulive
transformCode (ErlTuple [Atom "deallocate", n]:Atom "return":tl) acc =
  transformCode tl (Asm.ret un : acc)
  where
    Just un = intFromErl n
transformCode (ErlTuple [Atom "deallocate", n]:tl) acc =
  transformCode tl (Asm.deallocate un : acc)
  where
    Just un = intFromErl n
transformCode (Atom "return":tl) acc = transformCode tl (op : acc)
  where
    op = Asm.ret 0
-- {test,Cond,Fail,Ops}
transformCode (ErlTuple [Atom "test", Atom testName, fail1, ErlList args]:tl) acc =
  transformCode tl (op : acc)
  where
    ufail = parseLabel fail1
    uargs = map (fromJust . readLoc) args
    op = Asm.test testName ufail uargs Nothing WIgnore
-- {test,Cond,Fail,Live,Ops,Dst}
transformCode (ErlTuple [Atom "test", Atom testName, fail1, live, ErlList args, dst]:tl) acc =
  transformCode tl (op : acc)
  where
    ufail = parseLabel fail1
    uargs = map (fromJust . readLoc) args
    Just udst = writeLoc dst
    Just ulive = intFromErl live
    op = Asm.test testName ufail uargs (Just ulive) udst
-- {test,Cond,Fail,Src,Ops}
-- TODO
transformCode (ErlTuple (Atom callOp:_arity:mfa:deallc):tl) acc
  | callOp == "call_ext" ||
      callOp == "call_ext_only" || callOp == "call_ext_last" =
    transformCode tl (op : acc)
  where
    ErlTuple [Atom "extfunc", Atom m, Atom f, arity] = mfa
    Just uarity = intFromErl arity
    Just udeallc = intFromErl $ head deallc
    callType =
      case callOp of
        "call_ext"      -> NormalCall
        "call_ext_only" -> TailCall
        "call_ext_last" -> TailCallDealloc udeallc
        _               -> Uerlc.err "Bad call op type"
    op = Asm.callExt (m, f, uarity) callType
transformCode (ErlTuple (Atom callOp:arity:dst:deallc):tl) acc
  | callOp == "call" || callOp == "call_only" || callOp == "call_last" =
    transformCode tl (op : acc)
  where
    udst = parseLabel dst
    Just uarity = intFromErl arity
    [deallc0] = deallc
    Just udeallc = intFromErl deallc0
    callType =
      case callOp of
        "call"      -> NormalCall
        "call_only" -> TailCall
        "call_last" -> TailCallDealloc udeallc
        _           -> Uerlc.err "Bad call op type"
    op = Asm.callLabel uarity udst callType
transformCode (ErlTuple [Atom "call_fun", arity]:tl) acc =
  transformCode tl (op : acc)
  where
    Just uarity = intFromErl arity
    op = Asm.callFun uarity
transformCode (ErlTuple [Atom killOp, dst]:tl) acc
  | killOp == "kill" || killOp == "init" = transformCode tl (op : acc)
  where
    Just udst = writeLoc dst
    op = Asm.setNil udst
transformCode (ErlTuple [Atom "gc_bif", Atom bifName, onfail, live, ErlList args, dst]:tl) acc =
  transformCode tl (op : acc)
  where
    ufail = parseLabel onfail
    Just udst = writeLoc dst
    uargs = map (fromJust . readLoc) args
    Just ulive = intFromErl live
    op = Asm.callBif bifName ufail uargs (GcEnabledCall ulive) udst
transformCode (ErlTuple [Atom "bif", Atom bifName, onfail, ErlList args, dst]:tl) acc =
  transformCode tl (op : acc)
  where
    ufail = parseLabel onfail
    Just udst = writeLoc dst
    uargs = map (fromJust . readLoc) args
    op = Asm.callBif bifName ufail uargs NormalCall udst
transformCode (ErlTuple [Atom "test_heap", need, live]:tl) acc =
  transformCode tl (op : acc)
  where
    Just uneed = intFromErl need
    Just ulive = intFromErl live
    op = Asm.testHeap uneed ulive
transformCode (ErlTuple [Atom "trim", n, _remaining]:tl) acc =
  transformCode tl (op : acc)
  where
    Just un = intFromErl n
    op = Asm.trim un
transformCode (ErlTuple [Atom selOp, src, onfail, ErlTuple [Atom "list", ErlList choices]]:tl) acc
  | selOp == "select_val" || selOp == "select_tuple_arity" =
    transformCode tl (op : acc)
  where
    Just usrc = readLoc src
    ufail = parseLabel onfail
    uchoices = parseChoices choices []
    op =
      case selOp of
        "select_val" -> Asm.select SelectVal usrc ufail uchoices
        "select_tuple_arity" -> Asm.select SelectTupleArity usrc ufail uchoices
transformCode (ErlTuple [Atom "make_fun2", lbl, _indx, _olduniq, numfree]:tl) acc =
  transformCode tl (op : acc)
  where
    ulbl = parseLabel lbl
    Just unumfree = intFromErl numfree
    op = Asm.makeFun ulbl unumfree
transformCode (ErlTuple [Atom "bs_context_to_binary", src]:tl) acc =
  transformCode tl (op : acc)
  where
    Just usrc = readLoc src
    op = Asm.bsContextToBin usrc
transformCode (ErlTuple [Atom "bs_init2", onfail, sz, _extra, live, _flags, dst]:tl) acc =
  transformCode tl (op : acc)
  where
    Just usz = intFromErl sz
    Just ulive = intFromErl live
    Just udst = writeLoc dst
    uonfail = parseLabel onfail
    op = Asm.bsInit usz ulive udst uonfail
transformCode (ErlTuple [Atom bsOp, src, indx]:tl) acc
  | bsOp == "bs_save2" || bsOp == "bs_restore2" = transformCode tl (op : acc)
  where
    Just usrc = readLoc src
    Just uindx = intFromErl indx
    op =
      case bsOp of
        "bs_save2" -> Asm.bsSave usrc uindx
        "bs_restore2" -> Asm.bsRestore usrc uindx
        other -> Uerlc.err $ "can't create bs_ command for " ++ show other
transformCode (ErlTuple [Atom "bs_put_integer", _onfail, src, _n, flags, dst]:tl) acc =
  transformCode tl (op : acc)
  where
    Just usrc = readLoc src
    Just udst = writeLoc dst
    uflags = parseBinaryFlags flags
    op = Asm.bsPutInteger usrc uflags udst
transformCode (ErlTuple (Atom "%":_):tl) acc = transformCode tl acc
transformCode (other:_tl) _acc =
  Uerlc.err ("don't know how to transform " ++ show other)

parseBinaryFlags :: Term -> BinaryFlags
parseBinaryFlags (ErlTuple [Atom "field_flags", ErlList flgs]) =
  BinaryFlags unit sig big
  where
    unit = 8
    sig = Atom "signed" `elem` flgs
    big = Atom "big" `elem` flgs
parseBinaryFlags other =
  Uerlc.err $ "can't parse binary flags from " ++ show other
