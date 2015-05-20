{-# LANGUAGE UnicodeSyntax, FlexibleInstances #-}

module DeBruijn where

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

freeDBn :: DBn → [Int]
freeDBn = freeDBn' 0

freeDBn' :: Int → DBn → [Int]
freeDBn' d (DVar t)   = if t > d then [t - d] else []
freeDBn' d (DApp a b) = freeDBn' d a ++ freeDBn' d b
freeDBn' d (DAbs a)   = freeDBn' (d + 1) a



-- Term, depth
changeFree :: Int → (Int → Int) → DBn → DBn
changeFree d f o@(DVar t) = if t > d then DVar (f t) else o
changeFree d f (DApp a b) = DApp (changeFree d f a) (changeFree d f b)
changeFree d f (DAbs a)   = DAbs $ changeFree (d + 1) f a

incrementFree :: DBn → DBn
incrementFree = changeFree 1 (+1)

decrementFree :: DBn → DBn
decrementFree = changeFree 1 (\x → x - 1)

-- into what, what, depth (substituting when var == depth)
substituteDBn' :: DBn → DBn → Int → DBn
substituteDBn' (DVar t) w d | d == t = w
substituteDBn' o@(DVar t) _ _        = o
substituteDBn' (DApp a b) w d        = DApp (substituteDBn' a w d) (substituteDBn' b w d)
substituteDBn' (DAbs a) w d          = DAbs $ substituteDBn' a (incrementFree w) (d + 1)

substituteDBn :: DBn → DBn → DBn
substituteDBn a b = substituteDBn' a b 1

reduceDBn :: DBn → DBn
reduceDBn (DApp (DAbs a) b) = substituteDBn (decrementFree a) b
reduceDBn t                 = t

nfDBn :: DBn → DBn
nfDBn o@(DVar _)          = o
nfDBn (DAbs a)            = DAbs $ nfDBn a
nfDBn (DApp (DAbs a) b)   = nfDBn $ reduceDBn $ DApp (DAbs $ nfDBn a) $ nfDBn b
nfDBn (DApp a b)          = case nfDBn a of
  o@(DAbs _) → reduceDBn $ DApp o $ nfDBn b
  o          → DApp o $ nfDBn b
