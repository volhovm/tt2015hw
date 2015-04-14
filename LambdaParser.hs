{-# LANGUAGE UnicodeSyntax #-}
module LambdaParser where

import LambdaCalculus(Lambda(..))
import Text.ParserCombinators.Parsec
import Control.Applicative((<*>), (<$>))
--import Data.Functor.Compose

lambdaTerm :: Parser Lambda
lambdaTerm =  abstraction <|> application <|> atomic

parseTerm :: Parser Lambda
parseTerm = application <|> parseUnit

parseUnit :: Parser Lambda
parseUnit = (Var <$> variable) <|> abstraction <|> brackets parseTerm

application :: Parser Lambda
application = try $ do
  a ← parseUnit
  spaces
  b ← parseTerm
  return $ App a b

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
atomic = brackets lambdaTerm <|> (Var <$> variable)

variable :: Parser String
variable = (:) <$> try lower <*> many digit

parseLambda :: String → Either ParseError Lambda
parseLambda = parse parseTerm "Lambda parsing failed"
