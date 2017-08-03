module Pass.PassAsm
  ( transform
  ) where

import qualified Asm                  as A
import qualified Asm.Func             as AF
import qualified Asm.Mod              as AM
import qualified Bitcode             as B
import qualified Bitcode.Func        as BF
import qualified Bitcode.Mod         as BM
import qualified Bitcode.Op          as BO
import qualified Term                 as T
import           Uerlc

import qualified Control.Monad.Except as MEx
import qualified Control.Monad.State  as S
import qualified Data.Map             as Map

-- Given Asm module produce Bitcode module or throw an error
transform :: AM.Module -> BM.Module
transform amod = bcmod
  where
    bcmod0 = BM.new
    funs = Map.elems $ AM.funs amod
    bcmod =
      case transformM funs bcmod0 `MEx.catchError` Left of
        Right bcmod' -> bcmod'
        Left e       -> Uerlc.err $ show e

-- [monadic] given list of Asm funs and a Bitcode module, update module with
-- funs that are transformed to Bitcode funs
transformM :: [AF.Func] -> BM.Module -> CompileErrorOr BM.Module
transformM [] bcMod = Right bcMod
transformM (fun:fTail) bcMod0 = transformM fTail bcMod1
  where
    nameArity = AF.name fun
    bcMod1 = updateFun nameArity bcFun bcMod0
    bcFun =
      case S.evalState (transformFnM fun) bcMod0 of
        Right bcFun' -> bcFun'
        Left e       -> Uerlc.err $ show e

-- Updates/writes a func in a bitcode module, returns an updated module
updateFun :: T.FunArity -> BF.Func -> BM.Module -> BM.Module
updateFun nameArity f bcMod0 = bcMod1
  where
    funs0 = BM.funs bcMod0
    funs1 = Map.insert nameArity f funs0
    bcMod1 = bcMod0 {BM.funs = funs1}

-- [monadic] Given an Asm func converts it to a Bitcode func, also updates
-- the module with whatever is found on the way (atoms, literals etc)
transformFnM :: AF.Func -> BM.ModuleState (CompileErrorOr BF.Func)
transformFnM fn = do
  let asmCode = AF.code fn
  -- bitcode <- foldM foldOpHelper [] asmCode
  trResult <- transformAsmOpsM [] asmCode
  case trResult of
    Right bitcode ->
      let outFn = BF.Func {BF.bcfName = AF.name fn, BF.bcfCode = bitcode}
      in return (Right outFn)
    Left e -> return $ Left e

-- [monadic] Given an accumulator (bitcode ops) and input (a list of asm
-- opcodes) returns a list of bytecodes
transformAsmOpsM ::
     [BO.Instruction]
  -> [A.Instruction]
  -> BM.ModuleState (CompileErrorOr [BO.Instruction])
transformAsmOpsM acc [] = return $ Right (reverse acc)
transformAsmOpsM acc (aop:remainingAops) = do
  trResult <- transform1M aop
  case trResult of
    Right bcop -> transformAsmOpsM (bcop ++ acc) remainingAops
    Left e     -> return $ Left e

-- [monadic] For those cases when 1:1 simple mapping between asm and bitcode
-- is enough. For complex cases add a clause in transformAsmOpsM
transform1M :: A.Instruction -> BM.ModuleState (CompileErrorOr [BO.Instruction])
transform1M (A.AComment _s) = return $ Right []
transform1M (A.ALabel _lb) = return $ Right []
transform1M (A.ALine _ln) = return $ Right []
transform1M (A.AError e) = B.invokeErrorM e
transform1M (A.ATest tname onfail args maybeLive dst) =
  B.testM tname onfail args maybeLive dst
transform1M (A.AAlloc need live) = B.allocM need live
transform1M (A.AMove src dst) = B.moveM src dst
transform1M (A.ACall arity codeLoc callType) = B.callM arity codeLoc callType
transform1M (A.ATestHeap needH live) = B.testHeapM needH live
transform1M (A.ATupleNew sz dst) = B.tupleNewM sz dst
transform1M (A.ATuplePut val) = B.tuplePutM val
transform1M (A.ATupleGetEl src i dst) = B.tupleGetElM src i dst
transform1M (A.ATupleSetEl val index dst) = B.tupleSetElM val index dst
transform1M (A.ARet dealloc) = B.retM dealloc
transform1M (A.ACallBif name onfail args callType dst) =
  B.callBifM name onfail args callType dst
transform1M (A.ADecons src dstH dstT) = B.deconsM src dstH dstT
transform1M (A.ACons h t dst) = B.consM h t dst
transform1M (A.ASelect selType val onfail jtab) =
  B.selectM selType val onfail jtab
transform1M (A.AJump lbl) = B.jumpM lbl
transform1M (A.ACallFun arity) = B.callFunM arity
transform1M (A.ASetNil dst) = B.setNilM dst
transform1M (A.ATrim n) = B.trimM n
transform1M (A.AMakeFun lbl nfree) = B.makeFunM lbl nfree
transform1M (A.ABsContextToBin src) = B.bsContextToBinM src
transform1M (A.ABsSave src index) = B.bsSaveM src index
transform1M (A.ABsRestore src index) = B.bsRestoreM src index
transform1M (A.ABsInit sz gcLive dst onFail) = B.bsInitM sz gcLive dst onFail
transform1M (A.ABsPutInteger src bFlags dst) = B.bsPutIntegerM src bFlags dst
transform1M op = return $ Uerlc.errM $ "Don't know how to compile: " ++ show op
   -- return $ Right []
