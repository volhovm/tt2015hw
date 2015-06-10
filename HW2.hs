{-# LANGUAGE UnicodeSyntax #-}
module Main where

import System.IO
import LambdaCalculus
import Data.Set (elems)
import Data.List (intersperse)
import Parser(parseLambda)
import Utils

main :: IO ()
main = processIO $ \input output → do
  s ← hGetContents input
  putStrLn s
  case parseLambda s of
   Left e → putStrLn $ show e
   Right lambda → do putStrLn res
                     hPutStr output res
                     where res = intersperse '\n' $ concat $ elems $ freeVars lambda
