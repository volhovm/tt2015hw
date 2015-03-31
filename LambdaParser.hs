{-# LANGUAGE UnicodeSyntax #-}
module LambdaParser where

import LambdaCalculus(Lambda(..))
import Text.ParserCombinators.Parsec
import Control.Applicative((<*>), (<$>))
import Data.Functor.Compose

lambdaTerm = atomic <|> abstraction <|> application

application :: Parser Lambda
application = do
  p <- lambdaTerm
  space
  q <- lambdaTerm
  return $ App p q

abstraction :: Parser Lambda
abstraction = do
  try $ char '\\'
  v <- variable
  char '.'
  a <- lambdaTerm
  return $ Abs v a

brackets :: Parser a → Parser a
brackets s = do
  try $ char '('
  a <- s
  char ')'
  return a

atomic :: Parser Lambda
atomic = brackets lambdaTerm <|> (Var <$> variable)

variable :: Parser String
variable = (:) <$> try lower <*> many digit

parseLambda :: String → Either ParseError Lambda
parseLambda = parse lambdaTerm "Lambda parsing failed"
