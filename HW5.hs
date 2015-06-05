{-# LANGUAGE UnicodeSyntax #-}

module Main where

import System.IO
import Utils
import TermUnification
import Parser

main :: IO ()
main = processIO $ \input output → do
  s ← hGetContents input
  case parseTermsEqual s of
   Left e → putStrLn $ show e
   Right l → case unify l of
              Just u → hPutStrLn output $ unlines $ map show $ fillIDs u
              Nothing → hPutStrLn output "Failed to unify"
