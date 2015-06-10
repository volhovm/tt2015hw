{-# LANGUAGE UnicodeSyntax #-}
module Main where

import System.IO
import LambdaCalculus
import Parser(parseLambda)
import Utils

main :: IO ()
main = processIO $ \input output → do
  s ← hGetContents input
  putStrLn $ show s
  case parseLambda s of
   Left e → putStrLn $ show e
   Right lambda → do putStrLn $ show lambda
                     hPutStr output $ outputView lambda
