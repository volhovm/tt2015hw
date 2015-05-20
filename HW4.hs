{-# LANGUAGE UnicodeSyntax #-}

module Main where

import System.IO
import Utils
import LambdaCalculus
import DeBruijn

main :: IO ()
main = processIO $ \input output → do
  s ← hGetContents input
  return ()
