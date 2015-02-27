{-# LANGUAGE UnicodeSyntax #-}

module LambdaCalculus where

main :: IO ()
main = getLine >>= ( \x → putStrLn $ makeNicer x)


makeNicer :: String → String
makeNicer = (++) "input: "
