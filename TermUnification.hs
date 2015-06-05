{-# LANGUAGE UnicodeSyntax, TupleSections #-}

module TermUnification where

import Control.Applicative
import Control.Monad
import Data.List
import Data.Set (Set(..), fromList, singleton, unions, union, toList, difference)
import Data.Maybe

data Term = TFunc String [Term] | TVar String
                                  deriving Ord
instance Show Term where
  show (TVar s)       = s
  show (TFunc s list) = s ++ "(" ++ (unwords $ map show list) ++")"

instance Eq Term where
  (==) (TVar a) (TVar b)         = a == b
  (==) (TFunc a l1) (TFunc b l2) = a == b && all (\x -> fst x == snd x) (zip l1 l2)
  (==) _ _                       = False

data TermEq = TermEq Term Term
              deriving Ord

instance Show TermEq where
  show (TermEq a b) = show a ++ " = " ++ show b

instance Eq TermEq where
  (==) (TermEq a b) (TermEq c d) = a == c && b == d

-- what, where
containsT :: Term → Term → Bool
containsT a o@(TVar _)    = a == o
containsT a o@(TFunc _ l) = a == o || any (containsT a) l

containsTE :: Term → TermEq → Bool
containsTE a (TermEq b c) = containsT a b || containsT a c

-- substitution a b c, change all a to b in c
substituteT :: Term → Term → Term → Term
substituteT (TVar a) b (TVar c) | a == c = b
substituteT a@(TVar _) b (TFunc n c)     = TFunc n $ map (substituteT a b) c
substituteT (TVar _) _ c                 = c
-- can't substitute functions

substituteTE :: Term → Term → TermEq → TermEq
substituteTE a b (TermEq c d) = TermEq (substituteT a b c) (substituteT a b d)

swap :: TermEq → TermEq
swap (TermEq a@(TFunc _ _) b@(TVar _)) = TermEq b a
swap a                                 = a

samevar :: TermEq → Bool
samevar (TermEq (TVar a) (TVar b)) = a == b
samevar _                          = False

cmpFunc :: (String → String → Bool) → TermEq → Bool
cmpFunc op (TermEq (TFunc a _) (TFunc b _)) = op a b
cmpFunc _ _                                 = False

-- maps first occurence by predicate and adds it to list's head
replFst :: (Eq a) ⇒ (a → Bool) → (a → [a]) → [a] → Maybe [a]
replFst prd change list = let found = find prd list in
                                liftM2 (++) (change <$> found) (liftM2 delete found $ Just list)

termRed :: TermEq → [TermEq]
termRed (TermEq (TFunc _ l1) (TFunc _ l2)) = zipWith TermEq l1 l2

uniqCond :: [TermEq] → TermEq → Bool
uniqCond list t@(TermEq a@(TVar _) b) = b /= a && any (containsTE a) (delete t list)
uniqCond _ _                        = False

varSmth :: TermEq → Bool
varSmth (TermEq a@(TVar _) b) = a /= b
varSmth _                     = False

-- An Efficient Unification Algorithm (Martelli and Montanari)
unify :: [TermEq] → Maybe [TermEq]
unify list | any (\x → (swap x) /= x) list   = unify $ map swap list
unify list | any samevar list                = unify $ filter (not . samevar) list
unify list | any (cmpFunc (/=)) list         = Nothing
unify list | any (cmpFunc (==)) list         = unify =<< replFst (cmpFunc (==)) termRed list
unify list | any (uniqCond list) list        = unify =<<
               let x@(TermEq a@(TVar _) b) = fromJust $ find (uniqCond list) list in
                 if containsT a b
                 then Nothing
                 else Just $ x : map (substituteTE a b) (delete x list)
unify list                                   = Just list

unifyS :: [TermEq] → [TermEq]
unifyS = fromJust . unify

varsT :: Term → Set Term
varsT (TFunc _ b) = unions $ map varsT b
varsT a@(TVar _)  = singleton a

-- Adds variables pairs of form a = a if they're not present directly as a = ...
fillIDs :: [TermEq] → [TermEq]
fillIDs l = let right = unions $ map (\(TermEq _ b) → (varsT b)) l in
             let left = unions $ map (\(TermEq a _) → varsT a) l in
              (map (\x -> TermEq x x) $ toList (difference right left)) ++ l