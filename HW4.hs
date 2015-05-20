{-# LANGUAGE UnicodeSyntax #-}

module Main where

import System.IO
import Utils
import LambdaCalculus
import LambdaParser

main :: IO ()
main = processIO $ \input output → do
  s ← hGetContents input
  case parseLambda s of
   Left e  → putStrLn $ show e
   Right l → hPutStrLn output $ outputView $ nf l
