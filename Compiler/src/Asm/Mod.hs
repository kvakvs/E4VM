module Asm.Mod
  ( Module(..)
  ) where

import qualified Asm.Func  as AF
import qualified Term      as T

import           Data.List
import qualified Data.Map  as Map

data Module = Module
  { name :: String
  , funs :: Map.Map T.FunArity AF.Func
  , exports :: [T.FunArity]
  }

instance Show Module where
  show (Module name' funs' _exports) = intercalate "\n" [header, funs, footer]
    where
      header = ";; uAsm module " ++ name' ++ "======"
      footer = ";; ====== end uAsm module " ++ name'
      funs = intercalate "\n" strFuns
      strFuns = map show (Map.elems funs')
