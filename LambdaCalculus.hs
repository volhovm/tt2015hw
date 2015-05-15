{-# LANGUAGE UnicodeSyntax #-}

module LambdaCalculus where

import Data.Set (Set, empty, union, insert, singleton, member, toList)

type Literal = String
data Lambda = App Lambda Lambda | Abs Literal Lambda | Var Literal
instance Show Lambda where
  show (App a b) = show a ++ " " ++ show b
  show (Abs s l) = "λ" ++ s ++ ".(" ++ show l ++ ")"
  show (Var l) = l

outputView :: Lambda → String
outputView (Var s) = s
outputView (App a b) = brck $ (outputView a) ++ " " ++ outputView b
outputView (Abs s l) = brck $ '\\' : (s ++ "." ++ outputView l)
brck s = "(" ++ s ++ ")"


freeVars' :: Lambda → Set Literal → Set Literal
freeVars' (App a b) ignored = (freeVars' a ignored) `union` (freeVars' b ignored)
freeVars' (Abs s l) ignored = freeVars' l $ insert s ignored
freeVars' (Var s) ignored   = if member s ignored then empty else singleton s

freeVars :: Lambda → Set Literal
freeVars l = freeVars' l empty

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

reduce :: Lambda → Lambda
reduce o@(Var _) = o
reduce o@(App (Abs l x) t) = case substitute x l t of
  Left _  → o
  Right r → r
reduce (App a b) = App (reduce a) (reduce b)
reduce a = a

--substitute = substitute' empty
--
--substitute' :: Set Literal → Lambda → Literal → Lambda → Either Literal Lambda
--substitute' nonfree a@(Var l) var sub =
--  if not (var == l) then Right a else
--    case filter (\x → member x nonfree) (toList $ freeVars sub) of
--      []    → Right sub
--      (x:_) → Left x
--substitute' nonfree (App l r) var sub =
--  case ((substitute' nonfree l var sub), (substitute' nonfree r var sub)) of
--    (Right a, Right b) → Right $ App a b
--    (Left a, _)        → Left a
--    (_, b)             → b
--substitute' nonfree a@(Abs x l) var sub =
--  if x == var
--  then Right a
--  else case (substitute' (insert x nonfree) l var sub) of
--         Left y       → Left y
--         Right lambda → Right $ Abs x lambda
