module UFunction where

import           UAssembly

data Function = Function
  { ufunName :: String
  , ufunArity :: Integer
  , ufunBody :: [UAsmOp]
  }

instance Show Function where
  show (Function name' arity' body') =
    "\nFun{ " ++ name' ++ "/" ++ show arity' ++ ", " ++ show body' ++ " }\n"
