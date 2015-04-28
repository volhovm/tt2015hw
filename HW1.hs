{-# LANGUAGE UnicodeSyntax #-}

module Main where

import System.IO
import LambdaCalculus
import LambdaParser(parseLambda)
import Utils

main :: IO ()
main = processIO $ \input output → do
  s ← hGetContents input
  putStrLn $ show s
  case parseLambda s of
   Left e → putStrLn $ show e
   Right lambda → do putStrLn $ show lambda
                     hPutStr output $ simpleView lambda

simpleView :: Lambda → String
simpleView (Var s) = s
simpleView (App a b) = brackets $ (simpleView a) ++ " " ++ simpleView b
simpleView (Abs s l) = brackets $ '\\' : (s ++ "." ++ simpleView l)

brackets s = "(" ++ s ++ ")"
