{-# LANGUAGE UnicodeSyntax, FlexibleInstances #-}

module DeBruijn where

import LambdaCalculus (LambdaG(..), Lambda, Literal)
import Data.Map
import Data.Char

data DBn = DVar Int | DApp DBn DBn | DAbs DBn
instance Show DBn where
  show (DVar i) = show i
  show (DApp a b) = show a ++ " " ++ show b
  show (DAbs c) = "λ(" ++ show c ++ ")"

depth :: DBn → Int
depth (DVar _) = 1
depth (DApp a b) = max (depth a) $ depth b
depth (DAbs a) = (depth a) + 1

-- Conversions

-- M (var → depth), current depth, set from free char names to indeces, Lambda
lambdaToDBn' :: Map Literal Int → Int → Lambda → DBn
lambdaToDBn' m d (Var t) = if member t m
                             -- if bound
                             then DVar $ d - m ! t
                             -- if free
                             else DVar $ d + rename t + 1
lambdaToDBn' m d (App a b) = DApp (lambdaToDBn' m d a) (lambdaToDBn' m d b)
lambdaToDBn' m d (Abs l a) = DAbs $ lambdaToDBn' (insert l d m) (d + 1) a

lambdaToDBn :: Lambda → DBn
lambdaToDBn = lambdaToDBn' empty 0

--  Map [Lambda depth → current depth, current naming, (M (depth → var), DBn)
--dBnToLambda' :: Int → DBn → LambdaG Int
--dBnToLambda' d (DVar t) = if t > d then Var t else Var $ d - t
--dBnToLambda' d (DApp a b) = App (dBnToLambda' d a) (dBnToLambda' d b)
--dBnToLambda' d (DAbs a) = Abs d (dBnToLambda' (d + 1) a)

mapf :: Char → Int
mapf i | ord i >= 97 && ord i <= 122 = ord i - 97
mapf i | ord i >= 48 && ord i <= 57  = ord i - 48 + 26
mapf i | ord i == 39                 = 36

mapb :: Int → Char
mapb i | i < 26  = chr $ 97 + i
mapb i | i < 36  = chr $ 48 + i - 26
mapb i | i == 36 = '\''

renameBack :: Int → String
renameBack i | i >= 0 = mapb (i `mod` 37) : case (i `div` 37) of
                  0 → ""
                  n → renameBack n
renameBack i = "ERROR" ++ show i

renameBackChars :: Int → String
renameBackChars i | i >= 0 = chr (ord 'a' + i `mod` 26) : case (i `div` 26) of
                    0 → ""
                    n → renameBackChars n

rename :: String → Int
rename (x:[]) = mapf x
rename (x:xs) = mapf x + 37 * rename xs

dBnToLambda' :: Int → [String] → Map Int String → DBn → Lambda
dBnToLambda' d _ m (DVar t)      = Var $ if t > d then renameBack (t - d - 1) else m ! (d - t)
dBnToLambda' d x m (DApp a b)    = App (dBnToLambda' d x m a) (dBnToLambda' d x m b)
dBnToLambda' d (x:xs) m (DAbs a) = Abs x (dBnToLambda' (d + 1) xs (insert d x m) a)

freeDBn :: DBn → [Int]
freeDBn = freeDBn' 0

freeDBn' :: Int → DBn → [Int]
freeDBn' d (DVar t)   = if t > d then [t - d] else []
freeDBn' d (DApp a b) = freeDBn' d a ++ freeDBn' d b
freeDBn' d (DAbs a)   = freeDBn' (d + 1) a

dBnToLambda :: DBn → Lambda
dBnToLambda t = let possible n = renameBackChars n : possible (n + 1) in
                 let bounded t = [x | x ← possible 0, notElem (rename x + 1) $ freeDBn t] in
                  dBnToLambda' 0 (bounded t) empty t

-- Substitution

-- Term, depth, increment value
incrementFree :: DBn → Int → Int → DBn
incrementFree o@(DVar t) d n = if t > d then DVar (t + n) else o
incrementFree (DApp a b) d n = DApp (incrementFree a d n) (incrementFree b d n)
incrementFree (DAbs a)   d n = DAbs (incrementFree a (d + 1) n)

-- into what, what, depth
substituteDBn :: DBn → DBn → Int → DBn
substituteDBn o@(DVar t) w d
  | t == d    = w
  | otherwise = o
substituteDBn (DApp a b) w d = DApp (substituteDBn a w d) (substituteDBn b w d)
substituteDBn (DAbs a) w d   = DAbs $ substituteDBn a w $ d + 1

-- term, depth
maxFreeDepth :: DBn → Int → Int
maxFreeDepth (DVar t)   d = max d t
maxFreeDepth (DApp a b) d = max (maxFreeDepth a d) (maxFreeDepth b d)
maxFreeDepth (DAbs a)   d = maxFreeDepth a (d + 1)

reduceDBn :: DBn → DBn
reduceDBn t = reduceDBn' 0 (maxFreeDepth t 0) t

-- Current depth, maximum int of free vars in outer term, term
reduceDBn' :: Int → Int → DBn → DBn
reduceDBn' d n (DApp (DAbs a) b) = let reduced = substituteDBn a (incrementFree b 0 n) 1 in
                                    --reduceDBn' d (n + maxFreeDepth reduced 0) reduced
                                    reduced
reduceDBn' d n (DApp a b)        = DApp (reduceDBn' d n a) (reduceDBn' d n b)
reduceDBn' d n (DAbs a)          = DAbs (reduceDBn' (d + 1) n a)
reduceDBn' _ _ o@(DVar _)        = o
