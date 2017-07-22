module BytecodeMod where

import Term
import BytecodeFunc

import qualified Data.Map  as Map
import Data.List

data Module = Module
  { bcmName :: String,
    bcmAtoms :: Map.Map Int String,
    bcmLiterals :: Map.Map Int Term,
    bcmFuns :: Map.Map (String, Integer) BytecodeFunc.Function
  }

new = Module "" Map.empty Map.empty Map.empty

instance Show Module where
  show m =
    intercalate "\n" [header, funsText, footer]
    where
          name = bcmName m
          funs = bcmFuns m
          header = ";; bytecode module " ++ name ++ "======"
          footer = ";; ====== end bytecode module " ++ name
          funsText = intercalate "\n" strFuns
          strFuns = map show (Map.elems funs)
