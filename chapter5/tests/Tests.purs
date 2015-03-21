module Main where

import Data.Array
import Data.Maybe
import Debug.Trace
import Test.QuickCheck

import Chapter5

main = do
  trace "This is a placeholder test."
  quickCheck' 1 $ true == true
