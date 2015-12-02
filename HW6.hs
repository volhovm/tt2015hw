{-# LANGUAGE UnicodeSyntax #-}
module Main where

import System.IO
import Utils
import Parser
import Data.List (intercalate)
import TypeInference
import TermUnification

fromSystem :: [TermEq Type] → String
fromSystem c = unlines $ map formatTermEq c

formatTermEq :: TermEq Type → String
formatTermEq (TermEq a b) = (formatTerm a) ++ " = " ++ (formatTerm b)

formatTerm :: Term a → String
formatTerm (TFunc "→" terms) = "a(" ++ (intercalate ", " (map formatTerm terms)) ++ ")"
formatTerm (TVar a) = let ('τ':xs) = show a in
  "t" ++ xs

main :: IO()
main = processIO $ \input output → do
  s ← hGetContents input
  putStrLn ("Got: " ++ s)
  case parseLambda s of
   Right term → let (_,_,terms) = getSystem term in
                 do hPutStrLn stdout $ fromSystem terms
                    hPutStrLn stdout "Solution: "
                    case unify terms of
                     Just terms → hPutStrLn stdout $ fromSystem terms
                     Nothing → hPutStrLn stdout "Nothing"
   Left err → hPutStrLn stdout $ show err
--  hPutStrLn output $ case getType <$> parseLambda s of
--     Right (Just (t, m)) → (show t) ++ "\n" ++ unlines
--                           (map (\(l, lt) → l ++ " : " ++ show lt) m)
--     Right (Nothing)     → "Лямбда-выражение не имеет типа"
--     Left err            → "Parsing failed: " ++ show err
