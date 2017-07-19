{-# LANGUAGE UnicodeSyntax #-}
module UFunction where

data Function = Function
  { ufunName  :: String
  , ufunArity :: Integer
  , ufunBody  :: String
  }

instance Show Function where
  show (Function name' arity' body') =
    "Fun{ " ++ name' ++ "/" ++ show arity' ++ ", " ++ body' ++ " }\n"
