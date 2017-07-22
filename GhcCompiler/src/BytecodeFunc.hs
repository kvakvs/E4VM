module BytecodeFunc where

import           Data.List

data Function = Function
  { bcfName :: String
  , bcffArity :: Integer
  , bcfCode :: [Int]
  }

instance Show Function where
  show (Function name' arity' body') =
    intercalate "\n" ["", header, ops, footer, ""]
    where
      header = ";; bytecode fun " ++ funarity ++ " ------"
      footer = ";; ------ end bytecode " ++ funarity
      funarity = name' ++ "/" ++ show arity'
      indent2 t = "  " ++ t
      ops = intercalate "\n" $ map (indent2 . show) body'
