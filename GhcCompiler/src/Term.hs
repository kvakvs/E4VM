module Term
  ( Term(..)
  , strFromErl
  , bigintFromErl
  , intFromErl
  , funarityFromErl
  ) where

import           Data.List

data Term
  = Atom String
  | BinaryStr String
  | ErlComment String
  | ErlInt Integer
  | ErlList [Term]
  | ErlStr String
  | ErlTuple [Term]
  deriving(Eq)

instance Show Term where
  show (Atom s) = "'" ++ s ++ "'"
  show (ErlList items) =
    let str_items = map show items
    in "[" ++ intercalate "," str_items ++ "]"
  show (ErlTuple items) =
    let str_items = map show items
    in "{" ++ intercalate "," str_items ++ "}"
  show (ErlComment c) = "(% " ++ c ++ " %)"
  show (ErlInt i) = show i
  show (ErlStr s) = show s
  show (BinaryStr s) = "<<\"" ++ show s ++ "\">>"

-- unwrap a string from an Term Atom or ErlStr
strFromErl :: Term -> Maybe String
strFromErl (Atom s) = Just s
strFromErl (ErlStr s)  = Just s
strFromErl _sxpr     = Nothing

-- Unwrap a long integer from an SExpression
bigintFromErl :: Term -> Maybe Integer
bigintFromErl (ErlInt i) = Just i
bigintFromErl _sxpr    = Nothing

-- Unwrap a short machine integer from an SExpression
intFromErl :: Term -> Maybe Int
intFromErl (ErlInt i) = Just (fromIntegral i)
intFromErl _sxpr    = Nothing

-- Given two Term values from BEAM S parse produce a Funarity pair
funarityFromErl :: Term -> Term -> (String, Integer)
funarityFromErl fname farity =
  let Just fname' = strFromErl fname
      Just farity' = bigintFromErl farity
  in (fname', farity')
