module BeamSTypes where

import Data.List

data BeamSExpr = BeamSAtom String
               | BeamSList [BeamSExpr]
               | BeamSTuple [BeamSExpr]
               | BeamSInt Integer
               | BeamSString String
               | BeamSComment String


instance Show BeamSExpr where
  show (BeamSAtom s) = s

  show (BeamSList items) =
    let str_items = map show items
    in "[" ++ intercalate "," str_items ++ "]"

  show (BeamSTuple items) =
    let str_items = map show items
    in "{" ++ intercalate "," str_items ++ "}"

  show (BeamSComment c) = "(% " ++ c ++ " %)"

  show (BeamSInt i) = show i

  show (BeamSString s) = show s
