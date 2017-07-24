module BytecodeMod where

import           BytecodeFunc
import           Term

import           Data.List
import qualified Data.Map     as Map

data BcModule = BcModule
  { bcmName :: String
  , bcmAtoms :: Map.Map Int String
  , bcmLiterals :: Map.Map Int Term
  , bcmFuns :: Map.Map FunArity BcFunc
  }

new :: BcModule
new = BcModule "" Map.empty Map.empty Map.empty

instance Show BcModule where
  show m = intercalate "\n" [header, funsText, footer]
    where
      name = bcmName m
      funs = bcmFuns m
      header = ";; bytecode module " ++ name ++ "======"
      footer = ";; ====== end bytecode module " ++ name
      funsText = intercalate "\n" strFuns
      strFuns = map show (Map.elems funs)

bcmFindAtom :: BcModule -> String -> Maybe Int
bcmFindAtom m a = Just 0

bcmAddAtom :: BcModule -> String -> (BcModule, Int)
bcmAddAtom m a = (m, 0)
