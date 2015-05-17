{-# LANGUAGE UnicodeSyntax, FlexibleInstances #-}

module LambdaCalculus where

import Data.Char
import qualified Data.Set as S
import qualified Data.Map as M

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


-- M (var → depth), current depth, set from free char names to indeces, Lambda
lambdaToDBn' :: M.Map Literal Int → Int → M.Map String Int → Lambda → (M.Map String Int, DBn)
lambdaToDBn' m d f (Var t) = if M.member t m
                             -- if bound
                             then (f, DVar $ d - m M.! t)
                             -- if free
                             else if M.member t f
                                  -- take already assigned number
                                  then (f, DVar $ f M.! t)
                                  -- take max number in map + 1
                                  else (M.insert t nw f, DVar nw) where
                                    nw = (M.foldr max d f) + 1
lambdaToDBn' m d f (App a b) = let (f1, l1) = (lambdaToDBn' m d f a) in
                                let (f2, l2) = (lambdaToDBn' m d (M.union f f1) b) in
                                 (f2, DApp l1 l2)
lambdaToDBn' m d f (Abs l a) = let (nf, nl) = lambdaToDBn' (M.insert l d m) (d + 1) f a in
                                (nf, DAbs nl)

--  Map [Lambda depth → current depth, current naming, (M (depth → var), DBn)
dBnToLambda' :: Int → DBn → LambdaG Int
dBnToLambda' d (DVar t) = if t > d then Var t else Var $ d - t
dBnToLambda' d (DApp a b) = App (dBnToLambda' d a) (dBnToLambda' d b)
dBnToLambda' d (DAbs a) = Abs d (dBnToLambda' (d + 1) a)

rename :: LambdaG Int → Lambda
rename (Var t) = Var $ renameSingle t
rename (App a b) = App (rename a) (rename b)
rename (Abs l x) = Abs (renameSingle l) (rename x)

renameSingle :: Int → String
renameSingle 0 = ""
renameSingle i | i > 0 = chr ((ord 'a') + i `mod` 26) : renameSingle (i `div` 26)
renameSingle _ = "<lolwut>"

dBnToLambda :: DBn → Lambda
dBnToLambda = rename . dBnToLambda' 1

lambdaToDBn :: Lambda → DBn
lambdaToDBn = snd . lambdaToDBn' M.empty 1 M.empty

reduce :: Lambda → Lambda
reduce o@(Var _) = o
reduce o@(App (Abs l x) t) = case substitute x l t of
  Left _  → o
  Right r → r
reduce (App a b) = App (reduce a) (reduce b)
reduce a = a
