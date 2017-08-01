module Asm.Func
  ( Func(..)
  ) where

import qualified Asm             as A
import qualified Asm.Instruction as AI
import qualified Term            as T

import           Data.List

data Func = Func
  { name :: T.FunArity
  , code :: [AI.Instruction]
  }

instance Show Func where
  show (Func (T.FunArity name arity) body) =
    intercalate "\n" ["", header, ops, footer, ""]
    where
      header = ";; fun " ++ funarity ++ " ------"
      footer = ";; ------ end " ++ funarity
      funarity = name ++ "/" ++ show arity
      indent2 t = "  " ++ t
      ops = intercalate "\n" $ map (indent2 . show) body
