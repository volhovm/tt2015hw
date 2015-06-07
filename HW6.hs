{-# LANGUAGE UnicodeSyntax #-}

module Main where

import System.IO
import Utils
import TermUnification
import TypeInference
import Conversions

test :: (a → b) → a → b
test x y = x y

main :: IO()
main = processIO $ \input output → undefined
