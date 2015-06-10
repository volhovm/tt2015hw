{-# LANGUAGE UnicodeSyntax #-}
--http://www.cs.ucla.edu/~palsberg/course/cs239/reading/wand87.pdf
module TypeInference where

import TermUnification
import LambdaCalculus
import Control.Applicative
import Data.List((\\), nub)
import Data.Map(Map, (!), elems, insert, fromList)
import Data.Set(toList)

data Type = Atom String | Arrow Type Type
                          deriving (Eq, Ord)

instance Show Type where
  show (Atom s) = s
  show (Arrow l r) = "(" ++ show l ++ " → " ++ show r ++ ")"

data TypeState = TypeState [((Map Literal Type), Lambda, Type)] [TermEq Type]
               deriving Show

-- Convertions between Type and Term
fromType :: Type → Term Type
fromType s@(Atom _)    = TVar s
fromType (Arrow t1 t2) = TFunc "→" [fromType t1, fromType t2]

toType :: Term Type → Type
toType (TVar a)               = a
toType (TFunc "→" (t1:t2:[])) = Arrow (toType t1) (toType t2)

-- returns triple (type of the whole lambda in system, map freevars → types, system)
getSystem :: Lambda → (Type, [(Literal, Type)], [TermEq Type])
getSystem l = let vars = toList $ freeVars l in
               let ansType = atoms !! (length vars) in
                let zipped = zip vars atoms in
               (ansType,
                zipped,
                processState $ TypeState [(fromList zipped, l, ansType)] [])

-- returns all possible atoms (t0, t1, t2..)
atoms :: [Type]
atoms = map (\x → Atom $ 'τ' : show x) $ iterate (1+) 0

-- returns all atoms used in typestate
usedTypes :: TypeState → [Type]
usedTypes (TypeState g e) =
  let subtype (Arrow t1 t2) = (subtype t1) ++ (subtype t2)
      subtype atom          = [atom]
      in
  let inTermEq (TermEq a b) = map toType $ concat [varsT a, varsT b] in
  let inE = concatMap subtype $ concatMap inTermEq e in
  let in3Tuple (m, _, t) = (subtype t) ++ elems m in
   nub $ inE ++ concatMap in3Tuple g

newType :: TypeState → [Type]
newType = (((\\) atoms) . usedTypes)

processState :: TypeState → [TermEq Type]
processState (TypeState [] e) = e
processState state@(TypeState ((mp, l, t):g) e) = case l of
  Var c   → processState $ TypeState g $ (TermEq (fromType t) $ fromType $ mp ! c) : e
  App m n → let τ = head $ newType state in
             processState $ TypeState ((mp, m, Arrow τ t):(mp, n, τ):g) e
  Abs v m → let (τ1, τ2) = (head $ newType state, head $ tail $ newType state) in
             processState $ TypeState (((insert v τ1 mp), m, τ2):g)
                                      ((TermEq (fromType t) $ fromType $ Arrow τ1 τ2):e)

-- returns type and bindings for free variables
getType :: Lambda → Maybe (Type, [(Literal, Type)])
getType lambda = let (t0, free, set) = getSystem lambda in
  do ns ← fillIDs <$> (unify =<< swapByName <$> unify set)
     ans ← findAns t0 $ Just ns
     freeans ← mapM (\(l, t) → ((,) l) <$> toType <$> (findAns t $ Just ns)) free
     return $ (toType ans, freeans)
