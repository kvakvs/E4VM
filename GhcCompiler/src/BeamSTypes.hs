{-# LANGUAGE UnicodeSyntax #-}
module BeamSTypes
  ( SExpr(..)
  , sexprStr
  , sexprInt
  , sexprFunarity
  ) where

import           Data.List

data SExpr
  = SAtom String
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

-- unwrap a string from an SExpr SAtom or SStr
sexprStr ∷ SExpr → Maybe String
sexprStr (SAtom s) = Just s
sexprStr (SStr s)  = Just s
sexprStr _sxpr     = Nothing

-- Unwrap an integer from an SExpression
sexprInt ∷ SExpr → Maybe Integer
sexprInt (SInt i) = Just i
sexprInt _sxpr    = Nothing

-- Given two SExpr values from BEAM S parse produce a Funarity pair
sexprFunarity ∷ SExpr → SExpr → (String, Integer)
sexprFunarity fname farity =
  let Just fname' = sexprStr fname
      Just farity' = sexprInt farity
  in (fname', farity')
