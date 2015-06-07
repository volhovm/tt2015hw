{-# LANGUAGE UnicodeSyntax, FlexibleInstances #-}

module Conversions where

import LambdaCalculus
import DeBruijn
import Data.Map

dBnToLambda' :: Int → [String] → Map Int String → DBn → Lambda
dBnToLambda' d _ m (DVar t)      = Var $ if t > d then renameBack (t - d - 1) else m ! (d - t)
dBnToLambda' d x m (DApp a b)    = App (dBnToLambda' d x m a) (dBnToLambda' d x m b)
dBnToLambda' d (x:xs) m (DAbs a) = Abs x (dBnToLambda' (d + 1) xs (insert d x m) a)

dBnToLambda :: DBn → Lambda
dBnToLambda t = let possible n = renameBackChars n : possible (n + 1) in
                 let bounded t = [x | x ← possible 0, notElem (rename x + 1) $ freeDBn t] in
                  dBnToLambda' 0 (bounded t) empty t

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

nf :: Lambda → Lambda
nf = dBnToLambda . nfDBn . lambdaToDBn
