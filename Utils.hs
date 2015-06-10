{-# LANGUAGE UnicodeSyntax #-}
module Utils where

import System.IO
import System.Environment
import Parser
import TermUnification
import LambdaCalculus

processIO :: (Handle → Handle → IO()) → IO()
processIO handling = do
  (arg1:arg2:_) ← getArgs
  input ← openFile arg1 ReadMode
  output ← openFile arg2 WriteMode
  handling input output
  hClose input
  hClose output

-- simply parses lambda
pl :: String → Lambda
pl s = case parseLambda s of
  Left _ → undefined
  Right l → l

-- simply parses term equation
pte :: String → TermEq String
pte s = case parseTermEqual s of
  Left _ → undefined
  Right l → l
