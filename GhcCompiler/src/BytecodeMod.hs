module BytecodeMod where

import Term

import qualified Data.Map  as Map
import Data.List

data Module = Module
  { bcmName :: String,
    bcmAtoms :: Map.Map Int String,
    bcmLiterals :: Map.Map Int Term
  }
  deriving Show

--instance Show Module where
--  show (Module name' funs' _exports) =
--    intercalate "\n" [header, funs, footer]
--    where header = ";; module " ++ name' ++ "======"
--          footer = ";; ====== end module " ++ name'
--          funs = intercalate "\n" strFuns
--          strFuns = map show (Map.elems funs')

new = Module "" Map.empty Map.empty
