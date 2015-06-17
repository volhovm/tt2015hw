{-# LANGUAGE UnicodeSyntax #-}
module Main where

import LambdaCalculus
import Conversions

main :: IO ()
main = putStrLn $ "Must be empty: " ++ show test4

cf, cx :: String
cf = "b"
cx = "a"

vcx, vcf :: Lambda
vcx = Var cx
vcf = Var cf

(===) :: Lambda → Lambda → Bool
(===) a b = (lambdaToDBn a) == (lambdaToDBn b)

test4 :: [Int]
test4 = testInc ++ testAdd ++ testMul ++ testPow ++ testFact

churchn :: Int → Lambda
churchn n = if (n < 0) then churchn 0
            else Abs cf $ Abs cx $
                             (iterate (App (vcf)) $ vcx) !! n

lcurry2 :: (Lambda → t → Lambda) → t → Lambda
lcurry2 foo = \y → Abs "i" $ (foo (Var "i") y)

inc :: Lambda → Lambda
inc n = Abs cf $ Abs cx $ App (vcf) (App (App n (vcf)) (vcx))

test :: (Int → Bool) → Int → Int → [Int]
test foo num from = filter (not . foo) $ take num $ iterate (+1) from

testInc :: [Int]
testInc = test (\x → (nf $ inc (churchn x)) === (churchn $ x + 1)) 200 0

add :: Lambda → Lambda → Lambda
add a b = Abs cf $ Abs cx $ App (App a $ vcf) (App (App b $ vcf) $ vcx)

testAdd :: [Int]
testAdd = test (\x → (nf $ add (churchn x) $ churchn $ x + 2) === (churchn $ x + x + 2)) 100 0

mul :: Lambda → Lambda → Lambda
mul a b = Abs cf $ Abs cx $ App (App a (App b $ vcf)) $ vcx

testMul :: [Int]
testMul = test (\x → (nf $ mul (churchn x) $ churchn $ x * x + 2) ===
                     (churchn $ x * (x * x + 2))) 20 0

pow :: Lambda → Lambda → Lambda
pow a b = nf $ App b a

(***) :: Int → Int → Int
n *** m = (iterate (n*) $ 1) !! m

testPow :: [Int]
testPow = test (\x →
                 --trace (show x ++ " ^ " ++ show (x+2)) $
                     (nf $ pow (churchn x) (churchn $ x + 2)) ===
                     (churchn $ x *** (x + 2))) 5 0

cY, cI :: Lambda
cY = Abs cf (App (Abs cx (App (App vcf vcx) vcx)) (Abs cx (App (App vcf vcx) vcx)))
cI = Abs cf $ vcf

-- if then else
chooser :: Lambda → Lambda → Lambda → Lambda
chooser a b c = App (App a b) c

true, false :: Lambda
true  = Abs "a" (Abs "b" $ Var("a"))
false = Abs "a" (Abs "b" $ Var("b"))

isZero :: Lambda
isZero = Abs cf (App (App vcf (Abs cx false)) true)

app :: Lambda → Lambda → Lambda
app a b = nf $ App a b

-- fac n = chooser (isZero)
fact :: Int → Lambda
fact 0 = (churchn 1)
fact n = mul (churchn n) $ fact (n-1)

(!!!) :: Int → Int
(!!!) 0 = 1
(!!!) i = i * (!!!) (i-1)

testFact :: [Int]
testFact = test (\x → (nf $ fact x) === (churchn $ (!!!) x)) 8 0
