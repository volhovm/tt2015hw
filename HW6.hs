{-# LANGUAGE UnicodeSyntax #-}
module Main where

import System.IO
import Utils
import Parser
import TypeInference
import Control.Applicative

main :: IO()
main = processIO $ \input output → do
  s ← hGetContents input
  putStrLn ("Got: " ++ s)
  hPutStrLn output $ case getType <$> parseLambda s of
     Right (Just (t, m)) → (show t) ++ "\n" ++ unlines
                           (map (\(l, lt) → l ++ " : " ++ show lt) m)
     Right (Nothing)     → "Лямбда-выражение не имеет типа"
     Left err            → "Parsing failed: " ++ show err
