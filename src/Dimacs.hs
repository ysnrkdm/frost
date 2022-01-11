module Dimacs
  ( parseDIMACSFromFile,
    parseDIMACSString,
    DIMACS (..),
    CNF (..),
    Clause (..),
    Var (..),
    Literal (..),
  )
where

import System.Environment
import Text.Parsec (anyChar, char, many, many1, manyTill, newline, oneOf, space, string, (<|>))
import Text.Parsec.Prim (parse, skipMany)
import Text.Parsec.String (Parser, parseFromFile)

natural :: Parser Int
natural = do
  n <- oneOf ['1' .. '9']
  m <- many (oneOf ['0' .. '9'])
  return (read (n : m))

type VarId = Int

data Var = Var VarId deriving (Show, Eq)

data Literal = Not Var | Normal Var deriving (Show, Eq)

parseNotLiteral :: Parser Literal
parseNotLiteral = do
  char '-'
  n <- natural
  return (Not (Var n))

parseNormalLiteral :: Parser Literal
parseNormalLiteral = do
  n <- natural
  return (Normal (Var n))

parseLiteral :: Parser Literal
parseLiteral = parseNotLiteral <|> parseNormalLiteral

data Clause = EmptyClause | Clause [Literal] | UnitClause Literal deriving (Show, Eq)

parseEOL :: Parser Char
parseEOL = do
  space
  char '0'

parseLiterals :: Parser [Literal]
parseLiterals =
  do
    l <- parseLiteral
    space
    ls <- parseLiterals
    return (l : ls)
    <|> do
      char '0'
      return []

parseClause :: Parser Clause
parseClause = do
  ls <- parseLiterals
  newline
  case (length ls) of
    0 -> return EmptyClause
    1 -> return $ UnitClause (head ls)
    _ -> return $ Clause ls

data CNF = CNF [Clause] deriving (Show)

parseCNF :: Parser CNF
parseCNF = do
  cs <- many1 parseClause
  return (CNF cs)

parseComment :: Parser String
parseComment = do
  char 'c'
  space
  comment <- (manyTill anyChar newline)
  return comment

type VariableCount = Int

type ClauseCount = Int

data Header = Header VariableCount ClauseCount deriving (Show)

parseHeader :: Parser Header
parseHeader = do
  char 'p'
  space
  string "cnf"
  space
  vc <- natural
  space
  cc <- natural
  newline
  return (Header vc cc)

data DIMACS = DIMACS
  { _variableCount :: VariableCount,
    _clauseCount :: ClauseCount,
    _cnf :: CNF
  }
  deriving (Show)

parseDIMACS = do
  skipMany parseComment
  header <- parseHeader
  cnf <- parseCNF
  return cnf
  case header of
    (Header vc cc) ->
      return
        DIMACS
          { _variableCount = vc,
            _clauseCount = cc,
            _cnf = cnf
          }

parseDIMACSFromFile = parseFromFile parseDIMACS

parseDIMACSString = parse parseDIMACS ""
