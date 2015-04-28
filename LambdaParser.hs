{-# LANGUAGE UnicodeSyntax #-}
module LambdaParser where

import Text.ParserCombinators.Parsec
import LambdaCalculus(Lambda(..))
import Control.Applicative((<*>), (<$>))

parseTerm :: Parser Lambda
parseTerm = application <|> parseUnit

parseUnit :: Parser Lambda
parseUnit = (Var <$> variable) <|> abstraction <|> brackets parseTerm

lexem :: Parser a → Parser a
lexem p = do
  x ← p
  optional spaces
  return x

application :: Parser Lambda
application = try $ do
  a ← parseUnit
  b ← many1 (try $ space >> parseUnit)
  return $ foldl1 App (a : b)

abstraction :: Parser Lambda
abstraction = do
  try $ char '\\'
  v ← variable
  char '.'
  a ← parseTerm
  return $ Abs v a

brackets :: Parser a → Parser a
brackets s = do
  try $ char '('
  a ← s
  char ')'
  return a

atomic :: Parser Lambda
atomic = brackets parseTerm <|> (Var <$> variable)

variable :: Parser String
variable = (:) <$> (try lower) <*> (many digit)

parseLine :: Parser Lambda
parseLine = do
  t ← parseTerm
  spaces
  optional $ char '\n'
  return t

parseLambda :: String → Either ParseError Lambda
parseLambda s = parse parseLine "Lambda parsing failed" s
