{-# LANGUAGE UnicodeSyntax #-}

module LambdaCalculus where

import Data.Set (Set, empty, union, insert, singleton, member)

type Literal = String
data Lambda = App Lambda Lambda | Abs Literal Lambda | Var Literal
instance Show Lambda where
  show (App a b) = show a ++ " " ++ show b
  show (Abs s l) = "λ" ++ s ++ ".(" ++ show l ++ ")"
  show (Var l) = l

freeVars' :: Lambda → Set Literal → Set Literal
freeVars' (App a b) ignored = (freeVars' a ignored) `union` (freeVars' b ignored)
freeVars' (Abs s l) ignored = freeVars' l $ insert s ignored
freeVars' (Var s) ignored = if member s ignored then empty else singleton s

freeVars :: Lambda → Set Literal
freeVars l = freeVars' l empty
