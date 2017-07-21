module UModule where

import           UFunction

import qualified Data.Map  as Map
import Data.List

data Module = Module
  { umodName :: String
  , umodFuns :: Map.Map (String, Integer) Function
  , umodExports :: [(String, Integer)]
  }

instance Show Module where
  show (Module name' funs' _exports) =
    intercalate "\n" [header, funs, footer]
    where header = ";; module " ++ name' ++ "======"
          footer = ";; ====== end module " ++ name'
          funs = intercalate "\n" strFuns
          strFuns = map show (Map.elems funs')
