module AsmMod where

import           AsmFunc

import qualified Data.Map  as Map
import Data.List

data Module = Module
  { amName :: String
  , amFuns :: Map.Map (String, Integer) Function
  , amExports :: [(String, Integer)]
  }

instance Show Module where
  show (Module name' funs' _exports) =
    intercalate "\n" [header, funs, footer]
    where header = ";; module " ++ name' ++ "======"
          footer = ";; ====== end module " ++ name'
          funs = intercalate "\n" strFuns
          strFuns = map show (Map.elems funs')
