module Lindenmayer where

import Prelude ( Monad, Show
               , (+), (-), (*), (/), ($)
               , bind, map, return
               )

import Control.Monad.Eff (Eff(..))
import Data.Array (concatMap, foldM)
import Data.Int (toNumber)
import Graphics.Canvas ( Canvas(..)
                       , Context2D(..)
                       , lineTo
                       , moveTo
                       )
import Math (pow)

data Alphabet = L | R | F

instance showAlphabet :: Show Alphabet where
  show L = "L"
  show R = "R"
  show F = "F"

type Sentence = Array Alphabet

lsystem :: forall a c m s. (Monad m) =>
           c ->
           Array a ->
           (a -> Array a) ->
           (c -> s -> Array a -> Int -> m s) ->
           Int ->
           s ->
           m s
lsystem ctx initSentence rules render n initState = go initSentence n where
  go currSentence 0 = render ctx initState currSentence n
  go currSentence n = go (concatMap rules currSentence) (n-1)

type CanvasState =
  { x :: Number
  , y :: Number
  , t :: Number
  }

renderSentencePath :: Context2D ->
                      CanvasState ->
                      Sentence ->
                      Int ->
                      Eff (canvas :: Canvas) CanvasState
renderSentencePath ctx initState sentence n = do
  let dr = 360.0 / (pow 3.0 $ toNumber n)
  moveTo ctx initState.x initState.y
  foldM (\state a -> go state a dr) initState sentence where
    go state L _ = return $ state { t = state.t - Math.pi/3.0}
    go state R _ = return $ state { t = state.t + Math.pi/3.0}
    go state F dr = do
      let x' = state.x + Math.cos state.t * dr
          y' = state.y + Math.sin state.t * dr
      lineTo ctx x' y'
      return $ state { x = x', y = y' }
