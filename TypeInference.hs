{-# LANGUAGE UnicodeSyntax #-}
--http://www.cs.ucla.edu/~palsberg/course/cs239/reading/wand87.pdf
module TypeInference where

import TermUnification
import LambdaCalculus
import Conversions
import Control.Applicative
import Data.List((\\), nub, (!!), find)
import Data.Map(Map, (!), elems, insert, fromList)
import Data.Set(toList)

data Type = Atom String | Arrow Type Type
                          deriving Eq

instance Show Type where
  show (Atom s) = s
  show (Arrow l r) = "(" ++ show l ++ " → " ++ show r ++ ")"

data TypeState = TypeState [((Map Literal Type), Lambda, Type)] [TermEq Type]

fromType :: Type → Term Type
fromType s@(Atom _)    = TVar s
fromType (Arrow t1 t2) = TFunc "→" [fromType t1, fromType t2]

toType :: Term Type → Type
toType (TVar a)               = a
toType (TFunc "→" (t1:t2:[])) = Arrow (toType t1) (toType t2)

getSystem :: Lambda → (Type, [TermEq Type])
getSystem l = let vars = toList $ freeVars l in
               let ansType = atoms !! (length vars) in
               (ansType, processState $ TypeState [(fromList $ zip vars atoms, l, ansType)] [])

atoms :: [Type]
atoms = map (\x → Atom $ 't' : show x) $ iterate (1+) 0

usedTypes :: TypeState → [Type]
usedTypes (TypeState g e) =
  let inTermEq (TermEq a b) = map toType $ concat [varsT a, varsT b] in
  let inE = concat $ map inTermEq e in
  let in3Tuple (map, _, t) = t : elems map in
   nub $ inE ++ concatMap in3Tuple g

newType :: TypeState → [Type]
newType = (((\\) atoms) . usedTypes)

processState :: TypeState → [TermEq Type]
processState (TypeState [] e) = e
processState state@(TypeState ((map, l, t):g) e) = case l of
  Var c   → processState $ TypeState g $ (TermEq (fromType t) $ fromType $ map ! c) : e
  App m n → let τ = head $ newType state in
             processState $ TypeState ((map, m, Arrow τ t):(map, n, τ):g) e
  Abs v m → let (τ1, τ2) = (head $ newType state, head $ tail $ newType state) in
             processState $ TypeState (((insert v τ1 map), m, τ2):g)
                                      ((TermEq (fromType t) $ fromType $ Arrow τ1 τ2):e)


getType :: Lambda → Maybe Type
getType l = let (t0, set) = getSystem (nf l) in
  do ns ← unify set
     (TermEq a b) ← (find (\(TermEq (TVar t@(Atom s)) _) → t == t0)) ns
     return $ toType b
