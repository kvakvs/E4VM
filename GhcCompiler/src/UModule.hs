module UModule where

import UFunction

data Module = Module{
    umodName :: String,
    umodFuns :: [Function],
    umodExports :: [(String, Integer)]
  }

instance Show Module where
  show (Module name' funs' _exports) =
    "Module<" ++ name' ++ ">{" ++ show funs' ++ "}"
