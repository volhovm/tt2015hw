{-# LANGUAGE UnicodeSyntax #-}
module Main where

import System.IO
import Utils
import LambdaCalculus
import Parser(parseLambda)
import Conversions
import DeBruijn

main :: IO ()
main = processIO $ \input output → do
  s ← hGetContents input
  case parseLambda s of
   Left e  → putStrLn $ show e
   Right l → hPutStrLn output $ outputView $ (nf `seq` nf `seq` nf l)
--   Right l → hPutStrLn output $ show $ nfDBn $ lambdaToDBn l
