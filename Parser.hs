{-# LANGUAGE UnicodeSyntax #-}
module Parser where

import Text.ParserCombinators.Parsec
import LambdaCalculus
import TermUnification
import Control.Applicative((<*>), (<$>))

----- Global

-- anyOf must be here
dividers :: Parser ()
dividers = spaces <|> ((many $ char '\n') >> return ())

lexem :: Parser a → Parser a
lexem p = do
  optional dividers
  x ← p
  optional dividers
  return x

brackets :: Parser a → Parser a
brackets s = do
  try $ char '('
  a ← s
  char ')'
  return a

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
parseTerm = lexem $ application <|> parseUnit

parseUnit :: Parser Lambda
parseUnit = (Var <$> variable) <|> abstraction <|> brackets parseTerm

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

atomic :: Parser Lambda
atomic = brackets parseTerm <|> (Var <$> variable)

parseLambda :: String → Either ParseError Lambda
parseLambda = parse (line parseTerm) "Lambda parsing failed"

----- Terms unification

tfunc :: Parser String
tfunc = varGen $ oneOf "abcdefgh"

tvar :: Parser String
tvar = varGen $ oneOf "ijklmnopqrstuvwxyz"

parseTVar :: Parser Term
parseTVar = lexem $ TVar <$> try tvar

parseTFunc :: Parser Term
parseTFunc = lexem $ do
  name ← try tfunc
  args ← brackets $ many1 $ parseTVar <|> parseTFunc
  return $ TFunc name args

parseTEquation :: Parser TermEq
parseTEquation = let trm = parseTVar <|> parseTFunc in
                  do f ← trm
                     char '='
                     s ← trm
                     return $ TEquation f s

----- Miscellaneous

parseSubstitution :: String → Either ParseError (Lambda, Literal, Lambda)
parseSubstitution = parse (do t ← parseTerm
                              char '['
                              v ← variable
                              string ":="
                              s ← parseTerm
                              return $ (t, v, s)) "mda"
