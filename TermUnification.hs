{-# LANGUAGE UnicodeSyntax, ExistentialQuantification
 #-}
module TermUnification where

import Control.Applicative
import Control.Monad
import Data.List
import Data.Maybe

data Term a = TFunc String [Term a]
            | (Eq a, Show a) ⇒ TVar a

instance Show (Term a) where
  show (TVar s)       = show s
  show (TFunc s list) = s ++ "(" ++ (unwords $ map show list) ++")"

instance Eq (Term a) where
  (==) (TVar a) (TVar b)         = (==) b a
  (==) (TFunc a l1) (TFunc b l2) = a == b && all (\x → fst x == snd x) (zip l1 l2)
  (==) _ _                       = False

data TermEq a = TermEq (Term a) (Term a)

instance Show (TermEq a) where
  show (TermEq a b) = show a ++ " = " ++ show b

instance Eq (TermEq a) where
  (==) (TermEq a b) (TermEq c d) = a == c && b == d

-- what, where
containsT :: (Term a) → (Term a) → Bool
containsT a o@(TVar _)    = a == o
containsT a o@(TFunc _ l) = a == o || any (containsT a) l

containsTE :: Term a → TermEq a → Bool
containsTE a (TermEq b c) = containsT a b || containsT a c

-- substitution a b c, change all a to b in c
substituteT :: Term a → Term a → Term a → Term a
substituteT (TVar a) b (TVar c) | a == c = b
substituteT a@(TVar _) b (TFunc n c)     = TFunc n $ map (substituteT a b) c
substituteT (TVar _) _ c                 = c
-- can't substitute functions

substituteTE :: Term a → Term a → TermEq a → TermEq a
substituteTE a b (TermEq c d) = TermEq (substituteT a b c) (substituteT a b d)

swap :: TermEq a → TermEq a
swap (TermEq a@(TFunc _ _) b@(TVar _)) = TermEq b a
swap a                                 = a

samevar :: TermEq a → Bool
samevar (TermEq (TVar a) (TVar b)) = a == b
samevar _                          = False

cmpFunc :: (String → String → Bool) → TermEq a → Bool
cmpFunc op (TermEq (TFunc a _) (TFunc b _)) = op a b
cmpFunc _ _                                 = False

-- maps first occurence by predicate and adds it to list's head
replFst :: (Eq a) ⇒ (a → Bool) → (a → [a]) → [a] → Maybe [a]
replFst prd change list = let found = find prd list in
                                liftM2 (++) (change <$> found) (liftM2 delete found $ Just list)

termRed :: TermEq a → [TermEq a]
termRed (TermEq (TFunc _ l1) (TFunc _ l2)) = zipWith TermEq l1 l2

uniqCond :: [TermEq a] → TermEq a → Bool
uniqCond list t@(TermEq a@(TVar _) b) = b /= a && any (containsTE a) (delete t list)
uniqCond _ _                        = False

varSmth :: TermEq a → Bool
varSmth (TermEq a@(TVar _) b) = a /= b
varSmth _                     = False

-- An Efficient Unification Algorithm (Martelli and Montanari)
unify :: [TermEq a] → Maybe [TermEq a]
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

unifyS :: [TermEq a] → [TermEq a]
unifyS = fromJust . unify

varsT :: Term a → [Term a]
varsT (TFunc _ b) = nub $ concatMap varsT b
varsT a@(TVar _)  = [a]

-- Adds variables pairs of form a = a if they're not present directly as a = ...
fillIDs :: [TermEq a] → [TermEq a]
fillIDs l = let right = nub $ concatMap (\(TermEq _ b) → (varsT b)) l in
             let left = nub $ concatMap (\(TermEq a _) → varsT a) l in
              (map (\x -> TermEq x x) $ (right \\ left)) ++ l

-- swaps all variable pairs for least to be first
swapByName :: (Ord a) ⇒ [TermEq a] → [TermEq a]
swapByName l = let swp (TermEq x@(TVar a) y@(TVar b)) | a > b = TermEq y x
                   swp a                                      = a
                                                           in
                map swp l

-- finds TVar on the left in list and returns corresponding right term
findAns :: a → Maybe [TermEq a] → Maybe (Term a)
findAns f eq = do (TermEq _ b) ← (find (\(TermEq (TVar t) _) → t == f)) =<< eq
                  return b
