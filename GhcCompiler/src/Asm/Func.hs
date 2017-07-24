module Asm.Func where

import           Asm
import           Term

import           Data.List

data AFunc = AFunc
  { afName :: FunArity
  , afCode :: [UAsmOp]
  }

instance Show AFunc where
  show (AFunc (FunArity name arity) body) =
    intercalate "\n" ["", header, ops, footer, ""]
    where
      header = ";; fun " ++ funarity ++ " ------"
      footer = ";; ------ end " ++ funarity
      funarity = name ++ "/" ++ show arity
      indent2 t = "  " ++ t
      ops = intercalate "\n" $ map (indent2 . show) body
