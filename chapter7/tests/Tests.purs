module Main where

import Debug.Trace
import Test.QuickCheck

--import Chapter7

main = do
  trace "This is a placeholder test."
  quickCheck' 1 $ true == true
