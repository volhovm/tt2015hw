{-# LANGUAGE UnicodeSyntax, FlexibleInstances #-}

module LambdaCalculus where

import Data.Set
import qualified Data.Map as M
import DeBruijn

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


------ Conversions from/to De-Bruijn

dBnToLambda' :: Int → [String] → M.Map Int String → DBn → Lambda
dBnToLambda' d _ m (DVar t)      = Var $ if t > d then renameBack (t - d - 1) else m M.! (d - t)
dBnToLambda' d x m (DApp a b)    = App (dBnToLambda' d x m a) (dBnToLambda' d x m b)
dBnToLambda' d (x:xs) m (DAbs a) = Abs x (dBnToLambda' (d + 1) xs (M.insert d x m) a)

dBnToLambda :: DBn → Lambda
dBnToLambda t = let possible n = renameBackChars n : possible (n + 1) in
                 let bounded t = [x | x ← possible 0, notElem (rename x + 1) $ freeDBn t] in
                  dBnToLambda' 0 (bounded t) M.empty t

-- M (var → depth), current depth, set from free char names to indeces, Lambda
lambdaToDBn' :: M.Map Literal Int → Int → Lambda → DBn
lambdaToDBn' m d (Var t) = if M.member t m
                             -- if bound
                             then DVar $ d - m M.! t
                             -- if free
                             else DVar $ d + rename t + 1
lambdaToDBn' m d (App a b) = DApp (lambdaToDBn' m d a) (lambdaToDBn' m d b)
lambdaToDBn' m d (Abs l a) = DAbs $ lambdaToDBn' (M.insert l d m) (d + 1) a

lambdaToDBn :: Lambda → DBn
lambdaToDBn = lambdaToDBn' M.empty 0


nf :: Lambda → Lambda
nf = dBnToLambda . nfDBn . lambdaToDBn
