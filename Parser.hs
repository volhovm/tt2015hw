{-# LANGUAGE UnicodeSyntax #-}
module Parser where

import Text.ParserCombinators.Parsec
import LambdaCalculus
import TermUnification
import Control.Applicative((<*>), (<$>), (<*))

----- Global

lexem :: Parser a → Parser a
lexem = (>>) spaces

brackets :: Parser a → Parser a
brackets = between (char '(') (char ')')

varGen :: Parser Char → Parser String
varGen t = ((:) <$> (try t) <*> (many (digit <|> char '\'' <|> lower)))

variable :: Parser String
variable = varGen lower

line :: Parser a -> Parser a
line s = do
  t ← s
  spaces
  optional newline
  return t

----- Lambdas

parseTerm :: Parser Lambda
parseTerm = lexem $ (try application) <|> parseUnit

parseUnit :: Parser Lambda
parseUnit = (Var <$> variable) <|> abstraction <|> brackets parseTerm

application :: Parser Lambda
application = foldl1 App <$> sepBy1 (try parseUnit) (try spaces)

abstraction :: Parser Lambda
abstraction = do
  try $ char '\\'
  v ← variable
  char '.'
  a ← parseTerm
  return $ Abs v a

atomic :: Parser Lambda
atomic = brackets parseTerm <|> (Var <$> variable)

parseLambda :: String → Either ParseError Lambda
parseLambda = parse (parseTerm) "Lambda parsing failed"

----- Terms unification

tfunc :: Parser String
tfunc = varGen $ oneOf "abcdefgh"

tvar :: Parser String
tvar = varGen $ oneOf "ijklmnopqrstuvwxyz"

parseTVar :: Parser (Term String)
parseTVar = lexem $ TVar <$> try tvar

parseTFunc :: Parser (Term String)
parseTFunc = lexem $ do
  name ← try tfunc
  args ← brackets $ sepBy1 parseTTerm $ lexem $ char ','
  return $ TFunc name args

parseTTerm :: Parser (Term String)
parseTTerm = try parseTFunc <|> try parseTVar

parseTEq :: Parser (TermEq String)
parseTEq =  do f ← parseTTerm
               spaces >> char '='
               s ← parseTTerm
               return $ TermEq f s

parseTermEqual :: String → Either ParseError (TermEq String)
parseTermEqual = parse parseTEq "failed to parse equal terms"

parseTermsEqual :: String → Either ParseError [TermEq String]
parseTermsEqual = parse (sepBy1 parseTEq $ newline) "failed to parse many equalTerms"

----- Miscellaneous

parseSubstitution :: String → Either ParseError (Lambda, Literal, Lambda)
parseSubstitution = parse (do t ← parseTerm
                              char '['
                              v ← variable
                              string ":="
                              s ← parseTerm
                              return $ (t, v, s)) "mda"
