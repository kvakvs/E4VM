module Bitcode.Mod
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
  , profileOpcodeM
  ) where

import qualified Asm                    as A
import qualified Asm.Instruction        as AI
import qualified Bitcode.Encode.Huffman as H
import qualified Bitcode.Func           as BF
import qualified Bitcode.Op             as BO
import qualified Term                   as T

import qualified Control.Monad.State    as S
import           Data.List
import qualified Data.Map               as Map
import           Data.Word              (Word8)
import qualified Debug.Trace            as Dbg

data Module = Module
  { name :: String
  -- all used atoms, in a table
  , atoms :: Map.Map String Int
  -- all mentioned literals in a table
  , literals :: Map.Map T.Term Int
  -- external mfarities referred from the code, in a table
  , imports :: Map.Map T.MFArity Int
  -- collection of funs each with its own bitcode
  , funs :: Map.Map T.FunArity BF.Func
  -- jump tables for selectVal and selectTupleArity
  , jTabs :: [AI.JumpTab]
  -- Encoder which will be created after compiling finished from opcode
  -- frequencies and will be used to encode opcodes into smaller bit counts
  , huffmanEncoder :: Maybe (H.Encoder Word8)
  -- Opcodes frequencies calculated (enable this manually to generate data for
  -- B.Op.hardcodedFreq)
  , opStats :: Map.Map BO.Opcode Int
  }

type ModuleState = S.State Module

new :: Module
new =
  Module { name = ""
  , atoms = Map.empty
  , literals = Map.empty
  , imports = Map.empty
  , funs = Map.empty
  , jTabs = []
  , huffmanEncoder = Nothing
  , opStats = Map.empty
  }

instance Show Module where
  show m = intercalate "\n" [header, funsText, footer]
    where
      name' = name m
      funs' = funs m
      header = ";; bitcode module " ++ name' ++ "======"
      footer = ";; ====== end bitcode module " ++ name'
      funsText = intercalate "\n" strFuns
      strFuns = map show (Map.elems funs')

-- Pure find atom function
findAtom :: Module -> String -> Maybe Int
findAtom m a = Map.lookup a (atoms m)

-- Find atom and possibly add it
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

-- Find and possibly add literal
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

-- Find and possibly add import
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

addJumptabM :: AI.JumpTab -> S.State Module Int
addJumptabM jtab = do
  m <- S.get
  let oldJtabs = jTabs m
      index = length oldJtabs
      m1 = m {jTabs = oldJtabs ++ [jtab]}
  S.put m1
  return index

profileOpcodeM :: BO.Opcode -> S.State Module ()
profileOpcodeM op = do
  m0 <- S.get
  let s0 = opStats m0
      s1 = Map.insertWith (+) op 1 s0
  S.put $ m0 {opStats = s1}
  return ()

--profileOpcodeM :: BO.Opcode -> ModuleState ()
--profileOpcodeM op = do
--  m0 <- S.get
--  let m1 = profileOpcode m0 op
--  S.put m1
--  return ()
