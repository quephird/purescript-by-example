module Exercises1 where

--import Prelude (($), (+), (-), (*), (/), bind, map, return, unit)
import Prelude

import Control.Monad.Eff (Eff(..))
import Control.Monad.Eff.Console (CONSOLE(..), log)
import Data.Array ((..))
import Data.Int (toNumber)
import Data.List (List(..), toList)
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
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

-- TODO: * Think of better names for parameters
mapRange :: Number ->
            Number -> Number ->
            Number -> Number ->
            Number
mapRange value sourceStart sourceEnd targetStart targetEnd =
  targetStart + (targetEnd-targetStart)*(value-sourceStart)/(sourceEnd-sourceStart)

-- TODO: * Need to ask if all these returns are idiomatic
--       * Need to figure out how to determine width and height of canvas from ctx
--       * Add axes and ticks
plot :: forall eff. Context2D ->
                    (Number -> Number) ->
                    Tuple Number Number ->
                    Tuple Number Number ->
                    Int ->
                    Eff (canvas :: Canvas, console :: CONSOLE | eff) Unit
plot ctx f (Tuple fromX toX) (Tuple fromY toY) n = do
  log $ show fromX
  dx <- return $ (toX-fromX)/(toNumber n)
  xs <- return $ map (\i -> (toNumber i)*dx + fromX) $ 0..n
  realPts <- return $ map (\x -> Tuple x $ f x) xs
  pts <- return $ map (\(Tuple x y) -> { x: mapRange x fromX toX 0.0 800.0,
                                         y: mapRange y fromY toY 600.0 0.0 }) realPts
  renderPath ctx pts
  return unit

main = do
  Just canvas <- getCanvasElementById "canvas"
  ctx <- getContext2D canvas

  -- setStrokeStyle "#0000FF" ctx
  -- renderPath ctx [{ x: 100.0, y: 200.0 }
  --                ,{ x: 300.0, y: 300.0 }]

  setStrokeStyle "#FF00FF" ctx
  plot ctx (\x -> sin x) (Tuple (-6.0) 6.0) (Tuple (-1.5) 1.5) 50
