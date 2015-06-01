{-# LANGUAGE UnicodeSyntax #-}

module TermUnification where

data Term = TFunc [Term] | TVar String
data TermEq = Equation Term Term
