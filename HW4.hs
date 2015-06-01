{-# LANGUAGE UnicodeSyntax #-}

module Main where

import System.IO
import Utils
import LambdaCalculus
import Parser(parseLambda)
import Conversions
import DeBruijn

nf :: Lambda → Lambda
nf = dBnToLambda . nfDBn . lambdaToDBn

main :: IO ()
main = processIO $ \input output → do
  s ← hGetContents input
  case parseLambda s of
   Left e  → putStrLn $ show e
   Right l → hPutStrLn output $ outputView $ nf l
