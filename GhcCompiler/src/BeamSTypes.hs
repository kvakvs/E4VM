module BeamSTypes
  ( SExpr(..)
  , sexprStr
  , sexprInteger
  , sexprInt
  , sexprFunarity
  ) where

import           Data.List

data SExpr
  = SAtom String
  | SBinStr String
  | SComment String
  | SInt Integer
  | SList [SExpr]
  | SStr String
  | STuple [SExpr]
  deriving(Eq)

instance Show SExpr where
  show (SAtom s) = "'" ++ s ++ "'"
  show (SList items) =
    let str_items = map show items
    in "[" ++ intercalate "," str_items ++ "]"
  show (STuple items) =
    let str_items = map show items
    in "{" ++ intercalate "," str_items ++ "}"
  show (SComment c) = "(% " ++ c ++ " %)"
  show (SInt i) = show i
  show (SStr s) = show s
  show (SBinStr s) = "<<\"" ++ show s ++ "\">>"

-- unwrap a string from an SExpr SAtom or SStr
sexprStr :: SExpr -> Maybe String
sexprStr (SAtom s) = Just s
sexprStr (SStr s)  = Just s
sexprStr _sxpr     = Nothing

-- Unwrap a long integer from an SExpression
sexprInteger :: SExpr -> Maybe Integer
sexprInteger (SInt i) = Just i
sexprInteger _sxpr    = Nothing

-- Unwrap a short machine integer from an SExpression
sexprInt :: SExpr -> Maybe Int
sexprInt (SInt i) = Just (fromIntegral i)
sexprInt _sxpr    = Nothing

-- Given two SExpr values from BEAM S parse produce a Funarity pair
sexprFunarity :: SExpr -> SExpr -> (String, Integer)
sexprFunarity fname farity =
  let Just fname' = sexprStr fname
      Just farity' = sexprInteger farity
  in (fname', farity')
