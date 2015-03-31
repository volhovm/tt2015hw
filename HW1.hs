{-# LANGUAGE UnicodeSyntax #-}

module Main where

import LambdaCalculus
import LambdaParser(parseLambda)

main :: IO ()
main = do
       x <- getLine
       putStr $ case parseLambda x of
        Left er -> show er
        Right lambda -> show lambda

makeNicer :: String → String
makeNicer = (++) "input: "
