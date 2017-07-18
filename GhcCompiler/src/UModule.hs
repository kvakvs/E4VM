module UModule where

import UFunction

newtype Module = Module{funs :: [Function]}

instance Show Module where
  show (Module funs) =
    "Module{ " ++ show funs ++ " }"
