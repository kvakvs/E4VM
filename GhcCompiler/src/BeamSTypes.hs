module BeamSTypes (SExpr(..)) where

import Data.List

data SExpr = SAtom String
           | SList [SExpr]
           | STuple [SExpr]
           | SInt Integer
           | SStr String
           | SComment String


instance Show SExpr where
  show (SAtom s) = s

  show (SList items) =
    let str_items = map show items
    in "[" ++ intercalate "," str_items ++ "]"

  show (STuple items) =
    let str_items = map show items
    in "{" ++ intercalate "," str_items ++ "}"

  show (SComment c) = "(% " ++ c ++ " %)"

  show (SInt i) = show i

  show (SStr s) = show s
