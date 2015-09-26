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
                       , setShadowBlur
                       , setShadowColor
                       , setShadowOffsetX
                       , setShadowOffsetY
                       , setStrokeStyle
                       , strokePath
                       )

import Lindenmayer

kochInitSentence :: Sentence
kochInitSentence = [ F, R, R, F, R, R, F, R, R ]

kochRules :: Alphabet -> Sentence
kochRules L = [L]
kochRules R = [R]
kochRules F = [ F, L, F, R, R, F, L, F ]

sierpinskiInitSentence :: Sentence
sierpinskiInitSentence = [ M ]

sierpinskiRules :: Alphabet -> Sentence
sierpinskiRules L = [L]
sierpinskiRules R = [R]
sierpinskiRules F = [ F, L, M, L, F, R, M, R, F, R, M, R, F, L, M, L, F ]
sierpinskiRules M = [ M, R, F, R, M, L, F, L, M, L, F, L, M, R, F, R, M ]

main = do
  Just canvas <- getCanvasElementById "canvas"
  ctx <- getContext2D canvas
  setStrokeStyle "#007700" ctx
  setShadowBlur 10.0 ctx
  setShadowColor "#333333" ctx
  setShadowOffsetX 20.0 ctx
  setShadowOffsetY 20.0 ctx

  let initialState =
        { x: 67.0
        , y: 100.0
        , t: 0.0
        }
  -- TODO: Need to think about how to push the option to fill or stroke
  --         into the render function.
  strokePath ctx $ do
    lsystem ctx sierpinskiInitSentence sierpinskiRules renderSentencePath 4 initialState

  return unit
