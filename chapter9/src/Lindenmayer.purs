module Lindenmayer where

import Prelude ( Eq, Monad, Show
               , (+), (-), (*), (/), ($), (++), (==), (||)
               , bind, map, return, show
               )

import Control.Monad.Eff (Eff(..))
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

renderSentencePath :: Context2D ->
                      CanvasState ->
                      Sentence ->
                      Int ->
                      Eff (canvas :: Canvas) CanvasState
renderSentencePath ctx initState sentence n = do
  moveTo ctx initState.x initState.y
  foldM go initState sentence
  where
    --  TODO: Need to either derive the two magic numbers below
    --          or have specialized render functions for each lsystem path.
    dr = 200.0 / (pow 3.0 $ toNumber n)
    go state a =
      case a of
        L -> return $ state { t = state.t - Math.pi/3.0}
        R -> return $ state { t = state.t + Math.pi/3.0}
        _ -> do
          let x' = state.x + Math.cos state.t * dr
              y' = state.y + Math.sin state.t * dr
          lineTo ctx x' y'
          return $ state { x = x', y = y' }
