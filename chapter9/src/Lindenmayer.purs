module Lindenmayer where

import Prelude ( Eq, Monad, Show
               , (+), (-), (*), (/), ($), (++), (==), (||)
               , bind, map, return, show
               )

import Control.Monad.Eff (Eff(..))
import Control.Monad.Eff.Random (RANDOM(..), randomRange)
import Data.Array ((..), concatMap, foldM)
import Data.Int (toNumber)
import Data.Foldable (foldl)
import Graphics.Canvas ( Canvas(..)
                       , Context2D(..)
                       , lineTo
                       , moveTo
                       )
import Math (pow)

type Angle = Number

data Alphabet = L | R | F | M

-- So you can see things on the console; you don't get that for free.
instance showAlphabet :: Show Alphabet where
  show L = "L"
  show R = "R"
  show F = "F"
  show M = "M"

-- You don't get equality for free either!
instance eqAlphabet :: Eq Alphabet where
  eq L L = true
  eq R R = true
  eq F F = true
  eq M M = true
  eq _ _ = false

type Sentence = Array Alphabet

iterateSentence :: forall a.
                   (a -> Array a) ->
                   Array a ->
                   Int ->
                   Array a
iterateSentence rules initSentence n =
  foldl (\sentence _ -> concatMap rules sentence) initSentence (1..n)

lsystem :: forall a c m s. (Monad m) =>
           c ->
           Array a ->
           (a -> Array a) ->
           (c -> s -> Array a -> Int -> m s) ->
           Int ->
           s ->
           m s
lsystem ctx initSentence rules render n initState = do
  let finalSentence = iterateSentence rules initSentence n
  render ctx initState finalSentence n

type CanvasState =
  { x :: Number
  , y :: Number
  , t :: Number
  }

renderKoch :: Context2D ->
              CanvasState ->
              Sentence ->
              Int ->
              Eff (canvas :: Canvas, random :: RANDOM) CanvasState
renderKoch ctx initState sentence n = renderSentence' 3.0 ctx initState sentence n

renderSierpinski :: Context2D ->
                    CanvasState ->
                    Sentence ->
                    Int ->
                    Eff (canvas :: Canvas, random :: RANDOM) CanvasState
renderSierpinski ctx initState sentence n = renderSentence' 4.0 ctx initState sentence n

renderSentence' :: Number ->
                   Context2D ->
                   CanvasState ->
                   Sentence ->
                   Int ->
                   Eff (canvas :: Canvas, random :: RANDOM) CanvasState
renderSentence' scalingFactor ctx initState sentence n = do
  moveTo ctx initState.x initState.y
  foldM go initState sentence
  where
    --  TODO: Need to either derive the two magic numbers below
    --          or have specialized render functions for each lsystem path.
    dr = 360.0 / (pow scalingFactor $ toNumber n)
    go state a =
      case a of
        L -> return $ state { t = state.t - Math.pi/3.0}
        R -> return $ state { t = state.t + Math.pi/3.0}
        _ -> do
          f <- randomRange 0.6 1.4
          let x' = state.x + Math.cos state.t * dr * f
              y' = state.y + Math.sin state.t * dr * f
          lineTo ctx x' y'
          return $ state { x = x', y = y' }
