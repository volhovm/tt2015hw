{-# LANGUAGE UnicodeSyntax #-}
module LambdaParser where

import Text.ParserCombinators.Parsec
import LambdaCalculus
import Control.Applicative((<*>), (<$>))

-- anyOf must be here
dividers :: Parser ()
dividers = spaces <|> ((many $ char '\n') >> return ())

parseTerm :: Parser Lambda
parseTerm = lexem $ application <|> parseUnit

parseUnit :: Parser Lambda
parseUnit = (Var <$> variable) <|> abstraction <|> brackets parseTerm

lexem :: Parser a → Parser a
lexem p = do
  optional dividers
  x ← p
  optional dividers
  return x

application :: Parser Lambda
application = try $ do
  a ← parseUnit
  b ← many1 (try $ dividers >> parseUnit)
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
variable = (++) <$> ((:) <$> (try lower) <*> (many digit)) <*> (many $ char '\'')

parseLine :: Parser Lambda
parseLine = do
  t ← parseTerm
  spaces
  optional $ char '\n'
  return t

parseLambda :: String → Either ParseError Lambda
parseLambda = parse parseLine "Lambda parsing failed"

parseSubstitution :: String → Either ParseError (Lambda, Literal, Lambda)
parseSubstitution = parse (do t ← parseTerm
                              char '['
                              v ← variable
                              string ":="
                              s ← parseTerm
                              return $ (t, v, s)) "mda"
