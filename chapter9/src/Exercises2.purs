module Exercises2 where

import Prelude (($), (*), (++),  bind, return, unit)

import Control.Monad.Eff (Eff(..), forE)
import Control.Monad.Eff.Console (log)
import Control.Monad.Eff.DOM ( addEventListener
                             , querySelector)
import Control.Monad.Eff.Random (RANDOM(..), randomRange)
import Data.Int (fromNumber)
import Data.Maybe (Maybe(..))
import Data.Traversable (for)

import Graphics.Canvas ( arc
                       , fillPath
                       , getCanvasElementById
                       , getContext2D
                       , setFillStyle
                       , setStrokeStyle
                       , strokePath)
import Graphics.Drawing.Color ( colorString
                              , rgb)

randomColor = do
  r <- randomRange 0.0 256.0
  g <- randomRange 0.0 256.0
  b <- randomRange 0.0 256.0
  return $ colorString $ rgb r g b

randomCirclePath = do
  x <- randomRange 0.0 800.0
  y <- randomRange 0.0 600.0
  r <- randomRange 0.0 50.0
  return { x: x
         , y: y
         , r: r
         , start: 0.0
         , end: Math.pi * 2.0 }

strokeAndFillPath ctx path = do
  fillPath ctx path
  strokePath ctx path

main = do
  Just canvas <- getCanvasElementById "canvas"
  ctx <- getContext2D canvas

  node <- querySelector "#canvas"

  for node $ addEventListener "click" $ do
    fillColor <- randomColor
    strokeColor <- randomColor
    circlePath <- randomCirclePath
    setFillStyle fillColor ctx
    setStrokeStyle strokeColor ctx
    strokeAndFillPath ctx $ arc ctx circlePath

    return unit
