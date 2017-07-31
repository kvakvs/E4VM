module Bytecode.Mod
  ( Module(..)
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
import qualified Bytecode.Func       as BF
import qualified Term                as T

import qualified Control.Monad.State as S
import           Data.List
import qualified Data.Map            as Map

data Module = Module
  { name :: String
  , atoms :: Map.Map String Int
  , literals :: Map.Map T.Term Int
  , imports :: Map.Map T.MFArity Int
  , funs :: Map.Map T.FunArity BF.Func
  , jTabs :: [A.JumpTab]
  }

--  , bcmAtomCounter :: Int
--  , bcmLiteralCounter :: Int
--  , bcmImportCounter :: Int
new :: Module
new =
  Module
  { name = ""
  , atoms = Map.empty
  , literals = Map.empty
  , imports = Map.empty
  , funs = Map.empty
  , jTabs = []
  }

--  , bcmAtomCounter = 0
--  , bcmLiteralCounter = 0
--  , bcmImportCounter = 0
instance Show Module where
  show m = intercalate "\n" [header, funsText, footer]
    where
      name' = name m
      funs' = funs m
      header = ";; bytecode module " ++ name' ++ "======"
      footer = ";; ====== end bytecode module " ++ name'
      funsText = intercalate "\n" strFuns
      strFuns = map show (Map.elems funs')

-- Pure find atom function
bcmFindAtom :: Module -> String -> Maybe Int
bcmFindAtom m a = Map.lookup a (atoms m)

-- [monadic] Find atom and possibly add it
bcmFindAddAtomM :: String -> S.State Module Int
bcmFindAddAtomM a = do
  m <- S.get
  let lookupResult = Map.lookup a (atoms m)
  case lookupResult of
    Just i -> return i
    Nothing -> do
      let (m1, newI) = bcmAddAtom m a
      S.put m1
      return newI

-- Pure add atom function
bcmAddAtom :: Module -> String -> (Module, Int)
bcmAddAtom m a = (m1, counter)
  where
    oldAtoms = atoms m
    counter = Map.size oldAtoms
    newAtoms = Map.insert a counter oldAtoms
    m1 = m {atoms = newAtoms}

-- Pure lookup function
bcmFindLiteral :: Module -> T.Term -> Maybe Int
bcmFindLiteral m lit = Map.lookup lit (literals m)

-- Pure add literal function
bcmAddLiteral :: Module -> T.Term -> (Module, Int)
bcmAddLiteral m lit = (m1, counter)
  where
    oldLiterals = literals m
    counter = Map.size oldLiterals
    newLiterals = Map.insert lit counter oldLiterals
    m1 = m {literals = newLiterals}

-- [monadic] Find and possibly add literal
bcmFindAddLiteralM :: T.Term -> S.State Module Int
bcmFindAddLiteralM lit = do
  m <- S.get
  let lookupResult = Map.lookup lit (literals m)
  case lookupResult of
    Just i -> return i
    Nothing -> do
      let (m1, newI) = bcmAddLiteral m lit
      S.put m1
      return newI

-- Pure add Import function
bcmAddImport :: Module -> T.MFArity -> (Module, Int)
bcmAddImport m imp = (m1, counter)
  where
    oldImports = imports m
    counter = Map.size oldImports
    newImports = Map.insert imp counter oldImports
    m1 = m {imports = newImports}

-- Pure find import function
bcmFindImport :: Module -> T.MFArity -> Maybe Int
bcmFindImport m imp = Map.lookup imp (imports m)

-- [monadic] Find and possibly add import
bcmFindAddImportM :: T.MFArity -> S.State Module Int
bcmFindAddImportM imp = do
  m <- S.get
  let lookupResult = Map.lookup imp (imports m)
  case lookupResult of
    Just i -> return i
    Nothing -> do
      let (m1, newI) = bcmAddImport m imp
      S.put m1
      return newI

bcmAddJumptableM :: A.JumpTab -> S.State Module Int
bcmAddJumptableM jtab = do
  m <- S.get
  let oldJtabs = jTabs m
      index = length oldJtabs
      m1 = m {jTabs = oldJtabs ++ [jtab]}
  S.put m1
  return index
