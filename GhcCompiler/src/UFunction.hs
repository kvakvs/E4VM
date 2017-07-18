module UFunction where

data Function = Function{name :: String, arity :: Int}

instance Show Function where
  show (Function name' arity') =
    "Fun{ " ++ name' ++ "/" ++ show arity' ++ " }"
