module Bytecode.Mod
  ( BcModule(..)
  , new
  , bcmFindAtom
  , bcmAddAtom
  , bcmFindAddAtomM
  , bcmFindLiteral
  , bcmFindAddLiteralM
  , bcmAddLiteral
  , bcmFindImport
  , bcmAddImport
  , bcmFindAddImportM
  , bcmAddJumptableM
  ) where

import qualified Asm                 as A
import           Bytecode.Func
import qualified Term                as T

import qualified Control.Monad.State as S
import           Data.List
import qualified Data.Map            as Map

data BcModule = BcModule
  { bcmName :: String
  , bcmAtoms :: Map.Map String Int
  , bcmAtomCounter :: Int
  , bcmLiterals :: Map.Map T.Term Int
  , bcmLiteralCounter :: Int
  , bcmImports :: Map.Map T.MFArity Int
  , bcmImportCounter :: Int
  , bcmFuns :: Map.Map T.FunArity BcFunc
  , bcmJtabs :: [A.JumpTab]
  }

new :: BcModule
new =
  BcModule
  { bcmName = ""
  , bcmAtoms = Map.empty
  , bcmAtomCounter = 0
  , bcmLiterals = Map.empty
  , bcmLiteralCounter = 0
  , bcmImports = Map.empty
  , bcmImportCounter = 0
  , bcmFuns = Map.empty
  , bcmJtabs = []
  }

instance Show BcModule where
  show m = intercalate "\n" [header, funsText, footer]
    where
      name = bcmName m
      funs = bcmFuns m
      header = ";; bytecode module " ++ name ++ "======"
      footer = ";; ====== end bytecode module " ++ name
      funsText = intercalate "\n" strFuns
      strFuns = map show (Map.elems funs)

-- Pure find atom function
bcmFindAtom :: BcModule -> String -> Maybe Int
bcmFindAtom m a = Map.lookup a (bcmAtoms m)

-- [monadic] Find atom and possibly add it
bcmFindAddAtomM :: String -> S.State BcModule Int
bcmFindAddAtomM a = do
  m <- S.get
  let lookupResult = Map.lookup a (bcmAtoms m)
  case lookupResult of
    Just i -> return i
    Nothing -> do
      let (m1, newI) = bcmAddAtom m a
      S.put m1
      return newI

-- Pure add atom function
bcmAddAtom :: BcModule -> String -> (BcModule, Int)
bcmAddAtom m a = (m1, counter)
  where
    counter = bcmAtomCounter m + 1
    newAtoms = Map.insert a counter (bcmAtoms m)
    m1 = m {bcmAtoms = newAtoms, bcmAtomCounter = counter}

-- Pure lookup function
bcmFindLiteral :: BcModule -> T.Term -> Maybe Int
bcmFindLiteral m lit = Map.lookup lit (bcmLiterals m)

-- Pure add literal function
bcmAddLiteral :: BcModule -> T.Term -> (BcModule, Int)
bcmAddLiteral m lit = (m1, counter)
  where
    counter = bcmLiteralCounter m + 1
    newLiterals = Map.insert lit counter (bcmLiterals m)
    m1 = m {bcmLiterals = newLiterals, bcmLiteralCounter = counter}

-- [monadic] Find and possibly add literal
bcmFindAddLiteralM :: T.Term -> S.State BcModule Int
bcmFindAddLiteralM lit = do
  m <- S.get
  let lookupResult = Map.lookup lit (bcmLiterals m)
  case lookupResult of
    Just i -> return i
    Nothing -> do
      let (m1, newI) = bcmAddLiteral m lit
      S.put m1
      return newI

-- Pure add Import function
bcmAddImport :: BcModule -> T.MFArity -> (BcModule, Int)
bcmAddImport m imp = (m1, counter)
  where
    counter = bcmImportCounter m + 1
    newImports = Map.insert imp counter (bcmImports m)
    m1 = m {bcmImports = newImports, bcmImportCounter = counter}

-- Pure find import function
bcmFindImport :: BcModule -> T.MFArity -> Maybe Int
bcmFindImport m imp = Map.lookup imp (bcmImports m)

-- [monadic] Find and possibly add import
bcmFindAddImportM :: T.MFArity -> S.State BcModule Int
bcmFindAddImportM imp = do
  m <- S.get
  let lookupResult = Map.lookup imp (bcmImports m)
  case lookupResult of
    Just i -> return i
    Nothing -> do
      let (m1, newI) = bcmAddImport m imp
      S.put m1
      return newI

bcmAddJumptableM :: A.JumpTab -> S.State BcModule Int
bcmAddJumptableM jtab = do
  m <- S.get
  let oldJtabs = bcmJtabs m
      index = length oldJtabs
      m1 = m {bcmJtabs = oldJtabs ++ [jtab]}
  S.put m1
  return index
