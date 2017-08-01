module Asm
  ( allocate
  , badarg
  , badmatch
  , BinaryFlags(..)
  , bsContextToBin
  , bsInit
  , bsPutInteger
  , bsRestore
  , bsSave
  , BuiltinError(..)
  , callBif
  , callExt
  , callFun
  , callLabel
  , CallType(..)
  , caseClause
  , CodeLoc(..)
  , comment
  , cons
  , deallocate
  , decons
  , funcClause
  , ifClause
  , Instruction(..)
  , jump
  , JumpTab(..)
  , label
  , LabelLoc(..)
  , makeFun
  , move
  , ReadLoc(..)
  , ret
  , select
  , SelectSubj(..)
  , setNil
  , test
  , testHeap
  , trim
  , tupleGetEl
  , tupleNew
  , tuplePut
  , tupleSetEl
  , WriteLoc(..)
  ) where

import           Asm.Binary
import           Asm.Instruction
import           Asm.Locations
import qualified Term            as T

jump :: LabelLoc -> Instruction
jump = AJump

label :: Int -> Instruction
label i = ALabel (LabelLoc i)

comment :: Show a => a -> Instruction
comment x = AComment $ show x

ret :: Int -> Instruction
ret = ARet

move :: ReadLoc -> WriteLoc -> Instruction
move = AMove

funcClause :: Instruction
funcClause = AError EFunClause

caseClause :: Instruction
caseClause = AError ECaseClause

ifClause :: Instruction
ifClause = AError EIfClause

badarg :: Instruction
badarg = AError EBadArg

badmatch :: ReadLoc -> Instruction
badmatch r = AError (EBadMatch r)

tupleNew :: Int -> WriteLoc -> Instruction
tupleNew = ATupleNew

tuplePut :: ReadLoc -> Instruction
tuplePut = ATuplePut

tupleGetEl :: ReadLoc -> ReadLoc -> WriteLoc -> Instruction
tupleGetEl = ATupleGetEl

tupleSetEl :: ReadLoc -> ReadLoc -> WriteLoc -> Instruction
tupleSetEl = ATupleSetEl

testHeap :: Int -> Int -> Instruction
testHeap = ATestHeap

allocate :: Int -> Int -> Instruction
allocate = AAlloc

deallocate :: Int -> Instruction
deallocate = ADealloc

test :: String -> LabelLoc -> [ReadLoc] -> Maybe Int -> WriteLoc -> Instruction
test = ATest

callLabel :: Int -> LabelLoc -> CallType -> Instruction
callLabel arity lbl = ACall arity (CLabel lbl)

callExt :: (String, String, Int) -> CallType -> Instruction
callExt (m, f, arity) = ACall arity (CExtFunc m f arity)

callBif ::
     String -> LabelLoc -> [ReadLoc] -> CallType -> WriteLoc -> Instruction
callBif = ACallBif

decons :: ReadLoc -> WriteLoc -> WriteLoc -> Instruction
decons = ADecons

select :: SelectSubj -> ReadLoc -> LabelLoc -> JumpTab -> Instruction
select = ASelect

cons :: ReadLoc -> ReadLoc -> WriteLoc -> Instruction
cons = ACons

callFun :: Int -> Instruction
callFun = ACallFun

setNil :: WriteLoc -> Instruction
setNil = ASetNil

trim :: Int -> Instruction
trim = ATrim

makeFun :: LabelLoc -> Int -> Instruction
makeFun = AMakeFun

bsContextToBin :: ReadLoc -> Instruction
bsContextToBin = ABsContextToBin

bsSave :: ReadLoc -> Int -> Instruction
bsSave = ABsSave

bsRestore :: ReadLoc -> Int -> Instruction
bsRestore = ABsRestore

bsInit :: Int -> Int -> WriteLoc -> LabelLoc -> Instruction
bsInit = ABsInit

bsPutInteger :: ReadLoc -> BinaryFlags -> WriteLoc -> Instruction
bsPutInteger = ABsPutInteger
