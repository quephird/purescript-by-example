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
                       , fillText
                       , getCanvasElementById
                       , getContext2D
                       , lineTo
                       , moveTo
                       , rect
                       , setFillStyle
                       , setFont
                       , setStrokeStyle
                       , strokePath
                       )
import Math (exp, pow, sin)
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

data ScreenRange = FullScreen
                 | ScreenRange Number Number Number Number
type PlotOptions =
  { plotPoints :: Int
  , screenRange :: ScreenRange
  }
defaultPlotOptions :: PlotOptions
defaultPlotOptions =
  { plotPoints: 32
  , screenRange: FullScreen }

-- TODO: * Add axes and ticks
--       * Add title
plot :: forall eff. Context2D ->
                    (Number -> Number) ->
                    Tuple Number Number ->
                    Tuple Number Number ->
                    Eff (canvas :: Canvas | eff) Unit
plot ctx f rangeX rangeY = plotWithOptions ctx f rangeX rangeY defaultPlotOptions

plotWithOptions :: forall eff. Context2D ->
                               (Number -> Number) ->
                               Tuple Number Number ->
                               Tuple Number Number ->
                               PlotOptions ->
                               Eff (canvas :: Canvas | eff) Unit
plotWithOptions ctx f (Tuple fromX toX) (Tuple fromY toY) options = do
  renderPath ctx pts
  return unit where
    plotPoints = options.plotPoints
    screenRange = options.screenRange

    ctx' = coerceContext2D ctx
    screenW = ctx'.canvas.clientWidth
    screenH = ctx'.canvas.clientHeight

    dx = (toX-fromX) / (toNumber plotPoints)
    xs = map (\i -> (toNumber i)*dx + fromX) $ 0..plotPoints
    realPts = map (\x -> Tuple x $ f x) xs

    toScreenX screenW x =
      case screenRange of
        FullScreen                    -> mapRange x fromX toX 0.0 screenW
        (ScreenRange fromX' toX' _ _) -> mapRange x fromX' toX' 0.0 screenW
    toScreenY screenH y =
      case screenRange of
        FullScreen                    -> mapRange y fromY toY screenW 0.0
        (ScreenRange _ _ fromY' toY') -> mapRange y fromY' toY' screenW 0.0
    toScreenCoords screenW screenH (Tuple x y) = { x: toScreenX screenW x,
                                                   y: toScreenY screenH y}
    pts = map (toScreenCoords screenW screenH) realPts


main = do
  Just canvas <- getCanvasElementById "canvas"
  ctx <- getContext2D canvas

  -- setStrokeStyle "#0000FF" ctx
  -- renderPath ctx [{ x: 100.0, y: 200.0 }
  --                ,{ x: 300.0, y: 300.0 }]

  setFont "24pt Arial" ctx
  fillText ctx "sin(8x)^2 exp(-x^2)" 275.0 100.0

  setStrokeStyle "#FF0077" ctx
  plotWithOptions ctx
      (\x -> (pow (sin(8.0*x)) 2.0) * exp ((-1.0)*x*x))
      (Tuple (-3.0) 3.0)
      (Tuple (-0.5) 1.5)
      defaultPlotOptions { plotPoints = 200
                         , screenRange = ScreenRange (-6.0) 6.0 (-2.5) 2.5 }
