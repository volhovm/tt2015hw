{-# LANGUAGE UnicodeSyntax #-}

module Main where

import System.IO
import LambdaCalculus
import Data.Set (elems)
import Data.List (intersperse)
import LambdaParser(parseSubstitution)
import Utils

main :: IO ()
main = processIO $ \input output → do
  s ← hGetContents input
  putStrLn s
  case parseSubstitution s of
   Left e → putStrLn $ show e
   Right (l1, var, l2) → do putStrLn res
                            hPutStr output res
                            where res = case substitute l1 var l2 of
                                        Nothing → "Нет свободы для подстановки " ++
                                                  "для переменной " ++ var
                                        Just l  → show l
