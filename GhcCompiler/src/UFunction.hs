module UFunction where

import           UAssembly

import           Data.List

data Function = Function
  { ufunName :: String
  , ufunArity :: Integer
  , ufunBody :: [UAsmOp]
  }

instance Show Function where
  show (Function name' arity' body') =
    intercalate "\n" ["", header, ops, footer, ""]
    where
      header = ";; fun " ++ funarity ++ " ------"
      footer = ";; ------ end " ++ funarity
      funarity = name' ++ "/" ++ show arity'
      indent2 t = "  " ++ t
      ops = intercalate "\n" $ map (indent2 . show) body'
