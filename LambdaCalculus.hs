{-# LANGUAGE UnicodeSyntax, FlexibleInstances #-}
module LambdaCalculus where

import Data.Set

type Literal = String
data LambdaG a = App (LambdaG a) (LambdaG a) | Abs a (LambdaG a) | Var a
type Lambda = LambdaG Literal
instance Show Lambda where
  show (App a b) = show a ++ " " ++ show b
  show (Abs s l) = "λ" ++ s ++ ".(" ++ show l ++ ")"
  show (Var l) = l
instance Show (LambdaG Int) where
  show (App a b) = show a ++ " " ++ show b
  show (Abs s l) = "λ" ++ show s ++ ".(" ++ show l ++ ")"
  show (Var l) = show l


outputView :: Lambda → String
outputView (Var s) = s
outputView (App a b) = brck $ (outputView a) ++ " " ++ outputView b
outputView (Abs s l) = brck $ '\\' : (s ++ "." ++ outputView l)

brck :: String → String
brck s = "(" ++ s ++ ")"

-- returns set of free variables in lambda
freeVars :: Lambda → Set Literal
freeVars l = freeVars' l empty

freeVars' :: Lambda → Set Literal → Set Literal
freeVars' (App a b) ignored = (freeVars' a ignored) `union` (freeVars' b ignored)
freeVars' (Abs s l) ignored = freeVars' l $ insert s ignored
freeVars' (Var s) ignored   = if member s ignored then empty else singleton s

-- into, instead of what, what
substitute :: Lambda → Literal → Lambda → Either Literal Lambda
substitute o@(Var l) x a = Right (if (x == l) then a else o)
substitute (App l r) x a = case ((substitute l x a), (substitute r x a)) of
  (Right t, Right s) → Right $ App t s
  (Left t, Right _)  → Left t
  (_, Left t)        → Left t
substitute (Abs l r) x a = if or [x == l, member l (freeVars a)]
                           then Left l
                           else case substitute r x a of
                                 Left err    → Left err
                                 Right inner → Right $ Abs l inner
