module BeamSParser where

import BeamSTypes

import System.IO
import Control.Monad
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Expr
import Text.ParserCombinators.Parsec.Language
import qualified Text.ParserCombinators.Parsec.Token as Token


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


beamSParser :: Parser SExpr
beamSParser = whiteSpace >> sequenceOfExprs


sequenceOfExprs :: Parser SExpr
sequenceOfExprs = do
  forms <- many erlTerm
  return $ SList forms


erlComment :: Parser SExpr
erlComment = do
  _ <- string "%"
  c <- manyTill anyChar newline
  return $ SComment c


erlTerm :: Parser SExpr
erlTerm = do
  expr <- erlExpr
  _ <- char '.'
  whiteSpace
  optional erlComment
  return expr


erlExpr :: Parser SExpr
erlExpr = erlTuple
  <|> erlList
  <|> erlAtom
  <|> erlInteger
  <|> erlString


erlTuple :: Parser SExpr
erlTuple =
  braces erlTupleContent


erlTupleContent :: Parser SExpr
erlTupleContent = do
  items <- commaSep erlExpr
  return $ STuple items


erlList :: Parser SExpr
erlList =
  brackets erlListContent


erlListContent :: Parser SExpr
erlListContent = do
  items <- commaSep erlExpr
  return $ SList items


erlInteger :: Parser SExpr
erlInteger = do
  val <- integer
  return $ SInt val


erlAtom :: Parser SExpr
erlAtom =
  erlAtomStr <|> erlAtomQuoted


erlAtomStr :: Parser SExpr
erlAtomStr = do
  s <- identifier
  return $ SAtom s


erlAtomQuoted :: Parser SExpr
erlAtomQuoted = do
  whiteSpace
  _ <- char '\''
  s <- many (noneOf "'")
  _ <- char '\''
  return $ SAtom s


erlString :: Parser SExpr
erlString = do
  s <- stringLiteral
  return $ SStr s


identifier = Token.identifier lexer
braces = Token.braces lexer
brackets = Token.brackets lexer
commaSep = Token.commaSep lexer
integer = Token.integer lexer
whiteSpace = Token.whiteSpace lexer
dot = Token.dot lexer
stringLiteral = Token.stringLiteral lexer
charLiteral = Token.charLiteral lexer


parseS :: String -> Either String SExpr
parseS contents =
  case parse BeamSParser.beamSParser "" contents of
    Left parsecError -> Left $ show parsecError
    Right expr -> Right expr
