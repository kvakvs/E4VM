module Bytecode.Mod
  ( BcModule(..)
  , new
  , bcmFindAtom
  , bcmAddAtom
  , bcmFindLiteral
  , bcmAddLiteral
  ) where

import           Bytecode.Func
import           Term

import           Data.List
import qualified Data.Map      as Map

data BcModule = BcModule
  { bcmName :: String
  , bcmAtoms :: Map.Map String Int
  , bcmAtomCounter :: Int
  , bcmLiterals :: Map.Map Term Int
  , bcmLiteralCounter :: Int
  , bcmFuns :: Map.Map FunArity BcFunc
  }

new :: BcModule
new = BcModule "" Map.empty 0 Map.empty 0 Map.empty

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
bcmFindAtom m a = Map.lookup a (bcmAtoms m)

bcmAddAtom :: BcModule -> String -> (BcModule, Int)
bcmAddAtom m a = (m1, counter)
  where
    counter = bcmAtomCounter m + 1
    newAtoms = Map.insert a counter (bcmAtoms m)
    m1 = m {bcmAtoms = newAtoms, bcmAtomCounter = counter}

bcmFindLiteral :: BcModule -> Term -> Maybe Int
bcmFindLiteral m lit = Map.lookup lit (bcmLiterals m)

bcmAddLiteral :: BcModule -> Term -> (BcModule, Int)
bcmAddLiteral m lit = (m1, counter)
  where
    counter = bcmLiteralCounter m + 1
    newLiterals = Map.insert lit counter (bcmLiterals m)
    m1 = m {bcmLiterals = newLiterals, bcmLiteralCounter = counter}
