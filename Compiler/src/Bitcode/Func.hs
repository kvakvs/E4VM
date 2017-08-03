module Bitcode.Func
  ( Func(..)
  ) where

import qualified Bitcode.Op as BO
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
      header = ";; bitcode fun " ++ funarity ++ " ------"
      footer = ";; ------ end bitcode " ++ funarity
      funarity = name ++ "/" ++ show arity
      indent2 t = "  " ++ t
      ops = intercalate "\n" $ map (indent2 . show) body
