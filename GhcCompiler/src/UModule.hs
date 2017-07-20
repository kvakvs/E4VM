module UModule where

import           UFunction

import qualified Data.Map  as Map

data Module = Module
  { umodName :: String
  , umodFuns :: Map.Map (String, Integer) Function
  , umodExports :: [(String, Integer)]
  }

instance Show Module where
  show (Module name' funs' _exports) =
    "Module<" ++ name' ++ ">{" ++ show funs' ++ "}"
