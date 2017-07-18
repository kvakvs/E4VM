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


beamSParser :: Parser BeamSExpr
beamSParser = whiteSpace >> sequenceOfExprs


sequenceOfExprs :: Parser BeamSExpr
sequenceOfExprs = do
  forms <- many erlTerm
  return $ BeamSList forms


erlComment :: Parser BeamSExpr
erlComment = do
  _ <- string "%"
  c <- manyTill anyChar newline
  return $ BeamSComment c


erlTerm :: Parser BeamSExpr
erlTerm = do
  expr <- erlExpr
  _ <- char '.'
  whiteSpace
  optional erlComment
  return expr


erlExpr :: Parser BeamSExpr
erlExpr = erlTuple
  <|> erlList
  <|> erlAtom
  <|> erlInteger
  <|> erlString


erlTuple :: Parser BeamSExpr
erlTuple =
  braces erlTupleContent


erlTupleContent :: Parser BeamSExpr
erlTupleContent = do
  items <- commaSep erlExpr
  return $ BeamSTuple items


erlList :: Parser BeamSExpr
erlList =
  brackets erlListContent


erlListContent :: Parser BeamSExpr
erlListContent = do
  items <- commaSep erlExpr
  return $ BeamSList items


erlInteger :: Parser BeamSExpr
erlInteger = do
  val <- integer
  return $ BeamSInt val


erlAtom :: Parser BeamSExpr
erlAtom =
  erlAtomStr <|> erlAtomQuoted


erlAtomStr :: Parser BeamSExpr
erlAtomStr = do
  s <- identifier
  return $ BeamSAtom s


erlAtomQuoted :: Parser BeamSExpr
erlAtomQuoted = do
  whiteSpace
  _ <- char '\''
  s <- many (noneOf "'")
  _ <- char '\''
  return $ BeamSAtom s


erlString :: Parser BeamSExpr
erlString = do
  s <- stringLiteral
  return $ BeamSString s


identifier = Token.identifier lexer
braces = Token.braces lexer
brackets = Token.brackets lexer
commaSep = Token.commaSep lexer
integer = Token.integer lexer
whiteSpace = Token.whiteSpace lexer
dot = Token.dot lexer
stringLiteral = Token.stringLiteral lexer
charLiteral = Token.charLiteral lexer


parseS :: String -> Either String BeamSExpr
parseS contents =
  case parse BeamSParser.beamSParser "" contents of
    Left parsecError -> Left $ show parsecError
    Right expr -> Right expr
