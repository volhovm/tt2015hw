{-# LANGUAGE UnicodeSyntax #-}

module LambdaCalculus where

type Literal = String
data Lambda = App Lambda Lambda | Abs Literal Lambda | Var Literal
            deriving Show
