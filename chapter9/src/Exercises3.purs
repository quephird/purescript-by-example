module Exercises3 where

import Prelude ( (+), (-), (*), (/), ($)
               , bind, return, unit)

import Control.Monad.Eff (Eff(..))
import Data.Array (concatMap, foldM)
import Data.Maybe (Maybe(..))
import Graphics.Canvas ( Canvas(..)
                       , Context2D(..)
                       , closePath
                       , fillPath
                       , getCanvasElementById
                       , getContext2D
                       , lineTo
                       , moveTo
                       , setFillStyle
                       , strokePath
                       )

import Lindenmayer

kochInitSentence :: Sentence
kochInitSentence = [F, R, R, F, R, R, F, R, R]

kochRules :: Alphabet -> Sentence
kochRules L = [L]
kochRules R = [R]
kochRules F = [F, L, F, R, R, F, L, F]

main = do
  Just canvas <- getCanvasElementById "canvas"
  ctx <- getContext2D canvas
  let initialState =
        { x: 120.0
        , y: 200.0
        , t: 0.0
        }

  setFillStyle "#7700FF" ctx
  fillPath ctx $ do
    lsystem ctx kochInitSentence kochRules renderSentencePath 3 initialState
    closePath ctx

  return unit
