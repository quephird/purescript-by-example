module Main where

import Debug.Trace
import Test.QuickCheck

import Chapter4

main = do
  trace "Even numbers should be even."
  quickCheck' 100 $ \n -> even (2*Math.round n) == true

  trace "Odd numbers should be odd."
  quickCheck' 100 $ \n -> even (1+2*Math.round n) == false
