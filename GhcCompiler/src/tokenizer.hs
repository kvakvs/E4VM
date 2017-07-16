module Tokenizer where

import System.IO
import Control.Monad
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Expr
import Text.ParserCombinators.Parsec.Language
import qualified Text.ParserCombinators.Parsec.Token as Token
import Data.List

--newtype ErlAtom = MakeAtom String
--newtype ErlList = MakeErlList [Expr]
--newtype ErlTuple = MakeErlTuple [Expr]
--newtype ErlInt = MakeErlInt Integer
data Expr = ErlAtom String
  | ErlList [Expr]
  | ErlTuple [Expr]
  | ErlInt Integer
  | ErlString String
  | ErlComment String

instance Show Expr where
  show (ErlAtom s) = s

  show (ErlList items) =
    let str_items = map (\i -> show i) items
    in "[" ++ (intercalate "," str_items) ++ "]"

  show (ErlTuple items) =
    let str_items = map (\i -> show i) items
    in "{" ++ (intercalate "," str_items) ++ "}"

  show (ErlComment c) = ""

  show (ErlInt i) = show i

  show (ErlString s) = show s

languageDef =
  emptyDef { Token.commentLine     = "%"
           , Token.commentStart    = ""
           , Token.commentEnd      = ""
           , Token.identStart      = letter
           , Token.identLetter     = alphaNum <|> char '_'
           , Token.reservedNames   = []
           , Token.reservedOpNames = []
           , Token.opStart         = unexpected "operation not allowed"
           }

lexer = Token.makeTokenParser languageDef

beamSParser :: Parser Expr
beamSParser = whiteSpace >> sequenceOfExprs

--expr :: Parser Expr
--expr = expr' <|> sequenceOfExprs

sequenceOfExprs :: Parser Expr
sequenceOfExprs = do
  forms <- many erlTerm
  return $ ErlList forms

erlComment = do
  string "%"
  c <- manyTill anyChar newline
  whiteSpace
  return $ ErlComment c

erlTerm = do
  expr <- erlExpr
  char '.'
  whiteSpace
  optional erlComment
  return expr

erlExpr :: Parser Expr
erlExpr = erlTuple
  <|> erlList
  <|> erlAtom
  <|> erlInteger
  <|> erlString

erlTuple :: Parser Expr
erlTuple =
  braces erlTupleContent

erlTupleContent :: Parser Expr
erlTupleContent = do
  items <- commaSep erlExpr
  return $ ErlTuple items

erlList :: Parser Expr
erlList =
  brackets erlListContent

erlListContent :: Parser Expr
erlListContent = do
  items <- commaSep erlExpr
  return $ ErlList items

erlInteger :: Parser Expr
erlInteger = do
  val <- integer
  return $ ErlInt val

erlAtom = erlAtomStr <|> erlAtomQuoted

erlAtomStr :: Parser Expr
erlAtomStr = do
  s <- identifier
  return $ ErlAtom s

erlAtomQuoted :: Parser Expr
erlAtomQuoted = do
  whiteSpace
  char '\''
  s <- many (noneOf "'")
  char '\''
  return $ ErlAtom s

erlString :: Parser Expr
erlString = do
  s <- stringLiteral
  return $ ErlString s

identifier = Token.identifier lexer
braces = Token.braces lexer
brackets = Token.brackets lexer
commaSep = Token.commaSep lexer
integer = Token.integer lexer
whiteSpace = Token.whiteSpace lexer
dot = Token.dot lexer
stringLiteral = Token.stringLiteral lexer
charLiteral = Token.charLiteral lexer
