module Bytecode.Mod
  ( Module(..)
  , ModuleState
  , new
  , findAtom
  , addAtom
  , findAddAtomM
  , findLit
  , findAddLitM
  , addLit
  , findImport
  , addImport
  , findAddImport
  , addJumptabM
  ) where

import qualified Asm                 as A
import qualified Bytecode.Func       as BF
import qualified Term                as T

import qualified Control.Monad.State as S
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

type ModuleState = S.State Module

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
findAtom :: Module -> String -> Maybe Int
findAtom m a = Map.lookup a (atoms m)

-- [monadic] Find atom and possibly add it
findAddAtomM :: String -> S.State Module Int
findAddAtomM a = do
  m <- S.get
  let lookupResult = Map.lookup a (atoms m)
  case lookupResult of
    Just i -> return i
    Nothing -> do
      let (m1, newI) = addAtom m a
      S.put m1
      return newI

-- Pure add atom function
addAtom :: Module -> String -> (Module, Int)
addAtom m a = (m1, counter)
  where
    oldAtoms = atoms m
    counter = Map.size oldAtoms
    newAtoms = Map.insert a counter oldAtoms
    m1 = m {atoms = newAtoms}

-- Pure lookup function
findLit :: Module -> T.Term -> Maybe Int
findLit m lit = Map.lookup lit (literals m)

-- Pure add literal function
addLit :: Module -> T.Term -> (Module, Int)
addLit m lit = (m1, counter)
  where
    oldLiterals = literals m
    counter = Map.size oldLiterals
    newLiterals = Map.insert lit counter oldLiterals
    m1 = m {literals = newLiterals}

-- [monadic] Find and possibly add literal
findAddLitM :: T.Term -> S.State Module Int
findAddLitM lit = do
  m <- S.get
  let lookupResult = Map.lookup lit (literals m)
  case lookupResult of
    Just i -> return i
    Nothing -> do
      let (m1, newI) = addLit m lit
      S.put m1
      return newI

-- Pure add Import function
addImport :: Module -> T.MFArity -> (Module, Int)
addImport m imp = (m1, counter)
  where
    oldImports = imports m
    counter = Map.size oldImports
    newImports = Map.insert imp counter oldImports
    m1 = m {imports = newImports}

-- Pure find import function
findImport :: Module -> T.MFArity -> Maybe Int
findImport m imp = Map.lookup imp (imports m)

-- [monadic] Find and possibly add import
findAddImport :: T.MFArity -> S.State Module Int
findAddImport imp = do
  m <- S.get
  let lookupResult = Map.lookup imp (imports m)
  case lookupResult of
    Just i -> return i
    Nothing -> do
      let (m1, newI) = addImport m imp
      S.put m1
      return newI

addJumptabM :: A.JumpTab -> S.State Module Int
addJumptabM jtab = do
  m <- S.get
  let oldJtabs = jTabs m
      index = length oldJtabs
      m1 = m {jTabs = oldJtabs ++ [jtab]}
  S.put m1
  return index
