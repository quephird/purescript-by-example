module Exercises1 where

import Prelude (Unit(..), ($), (+), (-), (*), (/), bind, map, return, unit)

import Control.Monad.Eff (Eff(..))
import Control.Monad.Eff.Console (CONSOLE(..), log)
import Control.Monad.Eff.Console.Unsafe (logAny)
import Data.Array ((..))
import Data.Int (toNumber)
import Data.List (List(..), toList)
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Debug.Trace (traceAny)
import Graphics.Canvas ( Canvas(..)
                       , Context2D(..)
                       , closePath
                       , getCanvasElementById
                       , getContext2D
                       , lineTo
                       , moveTo
                       , rect
                       , setFillStyle
                       , setStrokeStyle
                       , strokePath)
import Math (sin)
import Unsafe.Coerce (unsafeCoerce)

type Point =
  { x :: Number
  , y :: Number
  }

renderPath :: forall eff. Context2D ->
                          Array Point ->
                          Eff (canvas :: Canvas | eff) Context2D
renderPath ctx pts = renderPath' $ toList pts where
  renderPath' pts' =
    case pts' of
      (Cons { x: x1, y: y1 }
            pts''@(Cons { x: x2, y: y2 } _)) -> do
        strokePath ctx $ do
          moveTo ctx x1 y1
          lineTo ctx x2 y2
        closePath ctx
        renderPath' pts''
      _ -> return ctx

-- These two types and one function are necessary to pluck out
-- the screen metrics through the Context2D object instead of
-- being burdened with passing the entire canvas object around.
type CanvasMini =
  { clientWidth :: Number
  , clientHeight :: Number
  }
type Context2DMini =
  { canvas :: CanvasMini
  }
coerceContext2D :: Context2D -> Context2DMini
coerceContext2D = unsafeCoerce

mapRange :: Number ->
            Number -> Number ->
            Number -> Number ->
            Number
mapRange val start1 end1 start2 end2 =
  start2 + (end2-start2) * (val-start1) / (end1-start1)

-- TODO: * Add axes and ticks
plot :: forall eff. Context2D ->
                    (Number -> Number) ->
                    Tuple Number Number ->
                    Tuple Number Number ->
                    Int ->
                    Eff (canvas :: Canvas, console :: CONSOLE | eff) Unit
plot ctx f (Tuple fromX toX) (Tuple fromY toY) n = do
  renderPath ctx pts
  return unit where
    dx = (toX-fromX) / (toNumber n)
    xs = map (\i -> (toNumber i)*dx + fromX) $ 0..n
    realPts = map (\x -> Tuple x $ f x) xs
    ctx' = coerceContext2D ctx
    screenW = ctx'.canvas.clientWidth
    screenH = ctx'.canvas.clientHeight
    toScreenX screenW x = mapRange x fromX toX 0.0 screenW
    toScreenY screenH y = mapRange y fromY toY screenH 0.0
    toScreenCoords screenW screenH (Tuple x y) = { x: toScreenX screenW x,
                                                   y: toScreenY screenH y}
    pts = map (toScreenCoords screenW screenH) realPts

main = do
  Just canvas <- getCanvasElementById "canvas"
  ctx <- getContext2D canvas

  -- setStrokeStyle "#0000FF" ctx
  -- renderPath ctx [{ x: 100.0, y: 200.0 }
  --                ,{ x: 300.0, y: 300.0 }]

  setStrokeStyle "#FF77FF" ctx
  plot ctx (\x -> sin x) (Tuple (-6.0) 6.0) (Tuple (-1.5) 1.5) 50
