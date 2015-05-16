{-# LANGUAGE UnicodeSyntax #-}

module LambdaCalculus where

import qualified Data.Set as S
import qualified Data.Map as M

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


freeVars' :: Lambda → S.Set Literal → S.Set Literal
freeVars' (App a b) ignored = (freeVars' a ignored) `S.union` (freeVars' b ignored)
freeVars' (Abs s l) ignored = freeVars' l $ S.insert s ignored
freeVars' (Var s) ignored   = if S.member s ignored then S.empty else S.singleton s

freeVars :: Lambda → S.Set Literal
freeVars l = freeVars' l S.empty

-- into, instead of what, what
substitute :: Lambda → Literal → Lambda → Either Literal Lambda
substitute o@(Var l) x a = Right (if (x == l) then a else o)
substitute (App l r) x a = case ((substitute l x a), (substitute r x a)) of
  (Right t, Right s) → Right $ App t s
  (Left t, Right _)  → Left t
  (_, Left t)        → Left t
substitute (Abs l r) x a = if or [x == l, S.member l (freeVars a)]
                           then Left l
                           else case substitute r x a of
                                 Left err    → Left err
                                 Right inner → Right $ Abs l inner


-- De Bruijn
data DBn = DVar Int | DApp DBn DBn | DAbs DBn
instance Show DBn where
  show (DVar i) = show i
  show (DApp a b) = show a ++ " " ++ show b
  show (DAbs c) = "λ(" ++ show c ++ ")"


-- M (var → depth), current depth, Lambda
lambdaToDBn' :: M.Map Literal Int → Int → Lambda → DBn
lambdaToDBn' m d (Var t) = DVar $ if M.member t m
                             then d - m M.! t
                             else d + 1
lambdaToDBn' m d (App a b) = DApp (lambdaToDBn' m d a) (lambdaToDBn' m d b)
lambdaToDBn' m d (Abs l a) = DAbs $ lambdaToDBn' (M.insert l d m) (d + 1) a


lambdaToDBn :: Lambda → DBn
lambdaToDBn = lambdaToDBn' M.empty 1

reduce :: Lambda → Lambda
reduce o@(Var _) = o
reduce o@(App (Abs l x) t) = case substitute x l t of
  Left _  → o
  Right r → r
reduce (App a b) = App (reduce a) (reduce b)
reduce a = a
