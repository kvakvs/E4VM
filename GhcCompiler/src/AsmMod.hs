module AsmMod where

import           AsmFunc
import           Term

import           Data.List
import qualified Data.Map  as Map

data AModule = AModule
  { amName :: String
  , amFuns :: Map.Map FunArity AFunc
  , amExports :: [FunArity]
  }

instance Show AModule where
  show (AModule name' funs' _exports) = intercalate "\n" [header, funs, footer]
    where
      header = ";; uAsm module " ++ name' ++ "======"
      footer = ";; ====== end uAsm module " ++ name'
      funs = intercalate "\n" strFuns
      strFuns = map show (Map.elems funs')
