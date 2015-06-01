{-# LANGUAGE UnicodeSyntax #-}

module TermUnification where

data Term = TFunc String [Term] | TVar String
instance Show Term where
  show (TVar s)       = s
  show (TFunc s list) = s ++ "(" ++ (unwords $ map show list) ++")"

data TermEq = TEquation Term Term
instance Show TermEq where
  show (TEquation a b) = show a ++ " = " ++ show b
