{-# LANGUAGE UnicodeSyntax #-}

module LambdaCalculus where

import Data.Set (Set, empty, union, insert, singleton, member, toList)

type Literal = String
data Lambda = App Lambda Lambda | Abs Literal Lambda | Var Literal
instance Show Lambda where
  show (App a b) = show a ++ " " ++ show b
  show (Abs s l) = "λ" ++ s ++ ".(" ++ show l ++ ")"
  show (Var l) = l

freeVars' :: Lambda → Set Literal → Set Literal
freeVars' (App a b) ignored = (freeVars' a ignored) `union` (freeVars' b ignored)
freeVars' (Abs s l) ignored = freeVars' l $ insert s ignored
freeVars' (Var s) ignored   = if member s ignored then empty else singleton s

freeVars :: Lambda → Set Literal
freeVars l = freeVars' l empty

-- into, instead of what, what
substitute :: Lambda → Literal → Lambda → Maybe Lambda
substitute = substitute' empty

substitute' :: Set Literal → Lambda → Literal → Lambda → Maybe Lambda
substitute' nonfree a@(Var l) var sub =
  if not (var == l) then Just a else
    if foldl (&&) True (map (\x → not $ member x nonfree) (toList $ freeVars sub))
    then Just sub
    else Nothing
substitute' nonfree (App l r) var sub =
  case ((substitute' nonfree l var sub), (substitute' nonfree r var sub)) of
    (Just a, Just b) → Just $ App a b
    _                → Nothing
substitute' nonfree a@(Abs x l) var sub =
  if x == var
  then Just a
  else case (substitute' (insert x nonfree) l var sub) of
         Nothing     → Nothing
         Just lambda → Just $ Abs x lambda
