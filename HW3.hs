{-# LANGUAGE UnicodeSyntax #-}

module Main where

import System.IO
import LambdaCalculus
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
                                        Left v → "Нет свободы для подстановки " ++
                                                  "для переменной " ++ v
                                        Right l  → outputView l
