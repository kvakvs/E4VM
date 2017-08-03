module Bytecode.Func
  ( Func(..)
  ) where

import qualified Bytecode.Op as BO
import           Data.List
import qualified Term        as T

data Func = Func
  { bcfName :: T.FunArity
  , bcfCode :: [BO.Instruction]
  }

instance Show Func where
  show (Func (T.FunArity name arity) body) =
    intercalate "\n" ["", header, ops, footer, ""]
    where
      header = ";; bytecode fun " ++ funarity ++ " ------"
      footer = ";; ------ end bytecode " ++ funarity
      funarity = name ++ "/" ++ show arity
      indent2 t = "  " ++ t
      ops = intercalate "\n" $ map (indent2 . show) body
