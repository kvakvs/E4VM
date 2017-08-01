-- Handles input from BeamSParser and creates an Module which has separate
-- functions, and each opcode is converted to some Asm
module Pass.PassBeamS
  ( transform
  ) where

import qualified Asm             as A
import qualified Asm.Func        as AF
import qualified Asm.Mod         as AM
import qualified Term            as T
import           Uerlc

import qualified Data.Map        as Map
import           Data.Maybe      (fromJust)

transform :: T.Term -> AM.Module
transform (T.ErlList l) = mod1
  where
    mod0 = AM.Module {AM.name = "", AM.funs = Map.empty, AM.exports = []}
    mod1 = transform' l mod0
transform other = Uerlc.err $ show other

-- Returns True if a tuple is a {function, ...} otherwise False
isBeamSFunction :: T.Term -> Bool
isBeamSFunction (T.ErlTuple (T.Atom "function":_)) = True
isBeamSFunction _                                  = False

-- Given list of tuples from BEAM S file handles header elements and then
-- takes functions one by one
transform' :: [T.Term] -> AM.Module -> AM.Module
transform' [] mod0 = mod0
transform' (T.ErlTuple [T.Atom "function", fname, farity, flabel]:tl) mod0 =
  transform' tl1 mod0 {AM.funs = funs1}
  where
    funs0 = AM.funs mod0
    tl1 = dropWhile (not . isBeamSFunction) tl
    fbody = takeWhile (not . isBeamSFunction) tl
    outFn = fnCreate fname farity flabel fbody
    funArity = T.funarityFromErl fname farity
    funs1 = Map.insert funArity outFn funs0
transform' (T.ErlTuple [T.Atom "module", T.Atom mname]:tl) mod0 =
  transform' tl mod1
  where
    mod1 = mod0 {AM.name = mname}
transform' (T.ErlTuple [T.Atom "exports", T.ErlList exps]:tl) mod0 =
  transform' tl mod1
  where
    exps1 = map (\(T.ErlTuple [fn, ar]) -> T.funarityFromErl fn ar) exps
    mod1 = mod0 {AM.exports = exps1}
-- ignored at the moment
transform' (T.ErlTuple [T.Atom "attributes", T.ErlList _mattr]:tl) mod0 =
  transform' tl mod0
transform' (T.ErlTuple [T.Atom "labels", _]:tl) mod0 = transform' tl mod0
transform' (form:_tl) _mod0 =
  Uerlc.err ("unexpected form in the input S file: " ++ show form)

-- Given F/Arity and code body return a Function object
fnCreate :: T.Term -> T.Term -> T.Term -> [T.Term] -> AF.Func
fnCreate (T.Atom fname) farity0 (T.ErlInt _flabel) fbody =
  AF.Func {AF.name = T.FunArity fname farity, AF.code = asmBody}
  where
    Just farity = T.intFromErl farity0
    asmBody = transformCode fbody []
fnCreate _f _a _label _body = Uerlc.err "parseFn expects a function"

-- Given a beamS expression, parse an Asm data source
readLoc :: T.Term -> Maybe A.ReadLoc
readLoc (T.ErlTuple [T.Atom "x", T.ErlInt x]) = Just $ A.RRegX (fromIntegral x)
readLoc (T.ErlTuple [T.Atom "y", T.ErlInt y]) = Just $ A.RRegY (fromIntegral y)
readLoc (T.ErlTuple [T.Atom "literal", lit]) = Just $ A.RLit lit
readLoc (T.ErlTuple [T.Atom "atom", T.Atom a]) = Just $ A.RAtom a
readLoc (T.ErlTuple [T.Atom "string", s]) = Just $ A.RLit s
readLoc (T.Atom "nil") = Just A.RNil
readLoc (T.ErlInt i) = Just $ A.RInt i
readLoc (T.ErlTuple [T.Atom "integer", T.ErlInt i]) = Just $ A.RInt i
readLoc f@(T.ErlTuple [T.Atom "field_flags", _]) = Just (A.RBinaryFlags bf)
  where
    bf = parseBinaryFlags f
readLoc other --Just $ ReadLocError $ show other
 = Just $ Uerlc.err $ "readLoc: not a readloc " ++ show other

-- Given a beamS expression parse an Asm data destination
writeLoc :: T.Term -> Maybe A.WriteLoc
writeLoc (T.ErlTuple [T.Atom "x", T.ErlInt x]) = Just $ A.WRegX (fromIntegral x)
writeLoc (T.ErlTuple [T.Atom "y", T.ErlInt y]) = Just $ A.WRegY (fromIntegral y)
writeLoc other --Just $ A.WriteLocError $ show other
 = Uerlc.err ("writeLoc: not a write loc " ++ show other)

parseLabel :: T.Term -> A.LabelLoc
parseLabel (T.ErlTuple [T.Atom "f", T.ErlInt 0]) = A.NoLabel
parseLabel (T.ErlTuple [T.Atom "f", T.ErlInt i]) = A.LabelLoc $ fromIntegral i
parseLabel other = Uerlc.err ("not a label" ++ show other)

parseChoices :: [T.Term] -> [(T.Term, A.LabelLoc)] -> [(T.Term, A.LabelLoc)]
parseChoices [] acc = reverse acc
parseChoices [_] _acc = Uerlc.err "parseChoices: given a list of odd length"
parseChoices (val:lbl:tl) acc = parseChoices tl acc1
  where
    ulbl = parseLabel lbl
    acc1 = (val, ulbl) : acc

parseBinaryFlags :: T.Term -> A.BinaryFlags
parseBinaryFlags (T.ErlTuple [T.Atom "field_flags", T.ErlList flgs]) =
  A.BinaryFlags unit sig big
  where
    unit = 8
    sig = T.Atom "signed" `elem` flgs
    big = T.Atom "big" `elem` flgs
parseBinaryFlags other =
  Uerlc.err $ "parseBinaryFlags: can't parse binary flags " ++ show other

transformCode :: [T.Term] -> [A.Instruction] -> [A.Instruction]
transformCode [] acc = reverse acc
transformCode (T.ErlTuple [T.Atom "label", f]:tl) acc =
  transformCode tl (op : acc)
  where
    Just nlabel = T.intFromErl f
    op = A.label nlabel
transformCode (T.ErlTuple [T.Atom "line", _]:tl) acc = transformCode tl acc
transformCode (T.ErlTuple [T.Atom "move", src, dst]:tl) acc =
  transformCode tl (A.move usrc udst : acc)
  where
    Just usrc = readLoc src
    Just udst = writeLoc dst
transformCode (T.ErlTuple [T.Atom "get_list", src, hddst, tldst]:tl) acc =
  transformCode tl (op : acc)
  where
    Just usrc = readLoc src
    Just uhd = writeLoc hddst
    Just utl = writeLoc tldst
    op = A.decons usrc uhd utl
transformCode (T.ErlTuple [T.Atom "put_list", h, t, dst]:tl) acc =
  transformCode tl (op : acc)
  where
    Just uhead = readLoc h
    Just utail = readLoc t
    Just udst = writeLoc dst
    op = A.cons uhead utail udst
transformCode (T.ErlTuple [T.Atom "func_info", _mod, _fun, _arity]:tl) acc =
  transformCode tl (A.funcClause : acc)
transformCode (T.ErlTuple [T.Atom "case_end", _dst]:tl) acc =
  transformCode tl (A.caseClause : acc)
transformCode (T.ErlTuple [T.Atom "if_end", _dst]:tl) acc =
  transformCode tl (A.ifClause : acc)
transformCode (T.ErlTuple [T.Atom "badmatch", val]:tl) acc =
  transformCode tl (op : acc)
  where
    Just uval = readLoc val
    op = A.badmatch uval
transformCode (T.ErlTuple [T.Atom "put_tuple", sz, dst]:tl) acc =
  transformCode tl (A.tupleNew usz udst : acc)
  where
    Just udst = writeLoc dst
    Just usz = T.intFromErl sz
transformCode (T.ErlTuple [T.Atom "put", val]:tl) acc =
  transformCode tl (A.tuplePut uval : acc)
  where
    Just uval = readLoc val
transformCode (T.ErlTuple [T.Atom "get_tuple_element", src, indx, dst]:tl) acc =
  transformCode tl (A.tupleGetEl usrc uindx udst : acc)
  where
    Just usrc = readLoc src
    Just uindx = readLoc indx
    Just udst = writeLoc dst
transformCode (T.ErlTuple [T.Atom "set_tuple_element", dst, tup, indx]:tl) acc =
  transformCode tl (op : acc)
  where
    Just uval = readLoc tup
    Just udst = writeLoc dst
    Just uindx = readLoc indx
    op = A.tupleSetEl uval uindx udst
transformCode (T.ErlTuple [T.Atom "jump", dst]:tl) acc =
  transformCode tl (op : acc)
  where
    udst = parseLabel dst
    op = A.jump udst
transformCode (T.ErlTuple [T.Atom opname, stkneed, live]:tl) acc
  | opname == "allocate" || opname == "allocate_zero" =
    transformCode tl (op : acc)
  where
    Just ustkneed = T.intFromErl stkneed
    Just ulive = T.intFromErl live
    op = A.allocate ustkneed ulive
transformCode (T.ErlTuple [T.Atom "deallocate", n]:T.Atom "return":tl) acc =
  transformCode tl (A.ret un : acc)
  where
    Just un = T.intFromErl n
transformCode (T.ErlTuple [T.Atom "deallocate", n]:tl) acc =
  transformCode tl (A.deallocate un : acc)
  where
    Just un = T.intFromErl n
transformCode (T.Atom "return":tl) acc = transformCode tl (op : acc)
  where
    op = A.ret 0
-- {test,Cond,Fail,Ops}
transformCode (T.ErlTuple [T.Atom "test", T.Atom testName, fail1, T.ErlList args]:tl) acc =
  transformCode tl (op : acc)
  where
    ufail = parseLabel fail1
    uargs = map (fromJust . readLoc) args
    op = A.test testName ufail uargs Nothing A.WIgnore
-- {test,Cond,Fail,Live,Ops,Dst}
transformCode (T.ErlTuple [T.Atom "test", T.Atom testName, fail1, live, T.ErlList args, dst]:tl) acc =
  transformCode tl (op : acc)
  where
    ufail = parseLabel fail1
    uargs = map (fromJust . readLoc) args
    Just udst = writeLoc dst
    Just ulive = T.intFromErl live
    op = A.test testName ufail uargs (Just ulive) udst
-- {test,Cond,Fail,Src,Ops}
-- TODO
transformCode (T.ErlTuple (T.Atom callOp:_arity:mfa:deallc):tl) acc
  | callOp == "call_ext" ||
      callOp == "call_ext_only" || callOp == "call_ext_last" =
    transformCode tl (op : acc)
  where
    T.ErlTuple [T.Atom "extfunc", T.Atom m, T.Atom f, arity] = mfa
    Just uarity = T.intFromErl arity
    Just udeallc = T.intFromErl $ head deallc
    callType =
      case callOp of
        "call_ext"      -> A.NormalCall
        "call_ext_only" -> A.TailCall
        "call_ext_last" -> A.TailCallDealloc udeallc
        _               -> Uerlc.err "Bad call op type"
    op = A.callExt (m, f, uarity) callType
transformCode (T.ErlTuple (T.Atom callOp:arity:dst:deallc):tl) acc
  | callOp == "call" || callOp == "call_only" || callOp == "call_last" =
    transformCode tl (op : acc)
  where
    udst = parseLabel dst
    Just uarity = T.intFromErl arity
    [deallc0] = deallc
    Just udeallc = T.intFromErl deallc0
    callType =
      case callOp of
        "call"      -> A.NormalCall
        "call_only" -> A.TailCall
        "call_last" -> A.TailCallDealloc udeallc
        _           -> Uerlc.err "Bad call op type"
    op = A.callLabel uarity udst callType
transformCode (T.ErlTuple [T.Atom "call_fun", arity]:tl) acc =
  transformCode tl (op : acc)
  where
    Just uarity = T.intFromErl arity
    op = A.callFun uarity
transformCode (T.ErlTuple [T.Atom killOp, dst]:tl) acc
  | killOp == "kill" || killOp == "init" = transformCode tl (op : acc)
  where
    Just udst = writeLoc dst
    op = A.setNil udst
transformCode (T.ErlTuple [T.Atom "gc_bif", T.Atom bifName, onfail, live, T.ErlList args, dst]:tl) acc =
  transformCode tl (op : acc)
  where
    ufail = parseLabel onfail
    Just udst = writeLoc dst
    uargs = map (fromJust . readLoc) args
    Just ulive = T.intFromErl live
    op = A.callBif bifName ufail uargs (A.GcEnabledCall ulive) udst
transformCode (T.ErlTuple [T.Atom "bif", T.Atom bifName, onfail, T.ErlList args, dst]:tl) acc =
  transformCode tl (op : acc)
  where
    ufail = parseLabel onfail
    Just udst = writeLoc dst
    uargs = map (fromJust . readLoc) args
    op = A.callBif bifName ufail uargs A.NormalCall udst
transformCode (T.ErlTuple [T.Atom "test_heap", need, live]:tl) acc =
  transformCode tl (op : acc)
  where
    Just uneed = T.intFromErl need
    Just ulive = T.intFromErl live
    op = A.testHeap uneed ulive
transformCode (T.ErlTuple [T.Atom "trim", n, _remaining]:tl) acc =
  transformCode tl (op : acc)
  where
    Just un = T.intFromErl n
    op = A.trim un
transformCode (T.ErlTuple [T.Atom selOp, src, onfail, T.ErlTuple [T.Atom "list", T.ErlList choices]]:tl) acc
  | selOp == "select_val" || selOp == "select_tuple_arity" =
    transformCode tl (op : acc)
  where
    Just usrc = readLoc src
    ufail = parseLabel onfail
    uchoices = parseChoices choices []
    op =
      case selOp of
        "select_val"         -> A.select A.SValue usrc ufail uchoices
        "select_tuple_arity" -> A.select A.STupleArity usrc ufail uchoices
transformCode (T.ErlTuple [T.Atom "make_fun2", lbl, _indx, _olduniq, numfree]:tl) acc =
  transformCode tl (op : acc)
  where
    ulbl = parseLabel lbl
    Just unumfree = T.intFromErl numfree
    op = A.makeFun ulbl unumfree
transformCode (T.ErlTuple [T.Atom "bs_context_to_binary", src]:tl) acc =
  transformCode tl (op : acc)
  where
    Just usrc = readLoc src
    op = A.bsContextToBin usrc
transformCode (T.ErlTuple [T.Atom "bs_init2", onfail, sz, _extra, live, _flags, dst]:tl) acc =
  transformCode tl (op : acc)
  where
    Just usz = T.intFromErl sz
    Just ulive = T.intFromErl live
    Just udst = writeLoc dst
    uonfail = parseLabel onfail
    op = A.bsInit usz ulive udst uonfail
transformCode (T.ErlTuple [T.Atom bsOp, src, indx]:tl) acc
  | bsOp == "bs_save2" || bsOp == "bs_restore2" = transformCode tl (op : acc)
  where
    Just usrc = readLoc src
    Just uindx = T.intFromErl indx
    op =
      case bsOp of
        "bs_save2" -> A.bsSave usrc uindx
        "bs_restore2" -> A.bsRestore usrc uindx
        other -> Uerlc.err $ "can't create bs_ command for " ++ show other
transformCode (T.ErlTuple [T.Atom "bs_put_integer", _onfail, src, _n, flags, dst]:tl) acc =
  transformCode tl (op : acc)
  where
    Just usrc = readLoc src
    Just udst = writeLoc dst
    uflags = parseBinaryFlags flags
    op = A.bsPutInteger usrc uflags udst
transformCode (T.ErlTuple (T.Atom "%":_):tl) acc = transformCode tl acc
transformCode (other:_tl) _acc =
  Uerlc.err ("don't know how to transform " ++ show other)
