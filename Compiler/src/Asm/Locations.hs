module Asm.Locations
  ( LabelLoc(..)
  , CodeLoc(..)
  , ReadLoc(..)
  , WriteLoc(..)
  ) where

import           Asm.Binary
import qualified Term       as T

data LabelLoc
  = LabelLoc Int
  | NoLabel

instance Show LabelLoc where
  show (LabelLoc i) = "-label:" ++ show i
  show NoLabel      = "-no-label"

-- sources of the data (registers, literals, immediates...)
data ReadLoc
  = RRegX Int
  | RRegY Int
  | RAtom String
  | RInt Integer
  | RLit T.Term
  | RNil
  | RBinaryFlags BinaryFlags

rarrow :: String
rarrow = "➚"

warrow :: String
warrow = "➘"

instance Show ReadLoc where
  show (RRegX i)         = "x" ++ show i ++ rarrow
  show (RRegY i)         = "y" ++ show i ++ rarrow
  show (RAtom a)         = show a
  show (RInt i)          = show i
  show (RLit lit)        = "lit:" ++ show lit
  show RNil              = "[]"
  show (RBinaryFlags bf) = "binflags:" ++ show bf

-- where you can possibly store the value
data WriteLoc
  = WRegX Int
  | WRegY Int
  | WIgnore

instance Show WriteLoc where
  show (WRegX i) = warrow ++ "x" ++ show i
  show (WRegY i) = warrow ++ "y" ++ show i
  show WIgnore   = warrow ++ "drop"

data CodeLoc
  = CLabel LabelLoc
  | CExtFunc String
             String
             Int

instance Show CodeLoc where
  show (CLabel ulbl)    = show ulbl
  show (CExtFunc m f a) = "@" ++ m ++ ":" ++ f ++ "/" ++ show a
