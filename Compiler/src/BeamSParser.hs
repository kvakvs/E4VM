module BeamSParser
  ( transform
  ) where

import           Term
import           Uerlc

import           Control.Monad
import           System.IO
import           Text.ParserCombinators.Parsec
import           Text.ParserCombinators.Parsec.Expr
import           Text.ParserCombinators.Parsec.Language
import qualified Text.ParserCombinators.Parsec.Token    as Token

languageDef =
  emptyDef
  { Token.commentLine = "%"
  , Token.commentStart = ""
  , Token.commentEnd = ""
  , Token.identStart = letter
  , Token.identLetter = alphaNum <|> char '_'
  , Token.reservedNames = []
  , Token.reservedOpNames = []
  , Token.opStart = unexpected "operation not allowed"
  }

lexer = Token.makeTokenParser languageDef

beamSParser :: Parser Term
beamSParser = whiteSpace >> sequenceOfExprs

sequenceOfExprs :: Parser Term
sequenceOfExprs = do
  forms <- many erlTerm
  return $ ErlList forms

erlComment :: Parser Term
erlComment = do
  _ <- string "%"
  c <- manyTill anyChar newline
  return $ ErlComment c

erlTerm :: Parser Term
erlTerm = do
  expr <- erlExpr
  _ <- char '.'
  whiteSpace
  optional erlComment
  return expr

erlExpr :: Parser Term
erlExpr =
  erlTuple <|> erlList <|> erlAtom <|> erlInteger <|> erlString <|> erlBinary

erlBinary :: Parser Term
erlBinary = do
  _ <- string "<<"
  s <- stringLiteral
  _ <- string ">>"
  return $ BinaryStr s

erlTuple :: Parser Term
erlTuple = braces erlTupleContent

erlTupleContent :: Parser Term
erlTupleContent = do
  items <- commaSep erlExpr
  return $ ErlTuple items

erlList :: Parser Term
erlList = brackets erlListContent

erlListContent :: Parser Term
erlListContent = do
  items <- commaSep erlExpr
  return $ ErlList items

erlInteger :: Parser Term
erlInteger = do
  val <- integer
  return $ ErlInt val

erlAtom :: Parser Term
erlAtom = erlAtomStr <|> erlAtomQuoted

erlAtomStr :: Parser Term
erlAtomStr = do
  s <- identifier
  return $ Atom s

erlAtomQuoted :: Parser Term
erlAtomQuoted = do
  whiteSpace
  _ <- char '\''
  s <- many (noneOf "'")
  _ <- char '\''
  return $ Atom s

erlString :: Parser Term
erlString = do
  s <- stringLiteral
  return $ ErlStr s

identifier = Token.identifier lexer

braces = Token.braces lexer

brackets = Token.brackets lexer

commaSep = Token.commaSep lexer

integer = Token.integer lexer

whiteSpace = Token.whiteSpace lexer

dot = Token.dot lexer

stringLiteral = Token.stringLiteral lexer

charLiteral = Token.charLiteral lexer

transform :: String -> Term
transform contents =
  case parse BeamSParser.beamSParser "" contents of
    Left parsecError -> Uerlc.err $ show parsecError
    Right expr       -> expr
