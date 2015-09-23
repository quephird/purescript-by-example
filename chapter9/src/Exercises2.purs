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
                       , canvasToDataURL
                       , clearRect
                       , drawImage
                       , fillPath
                       , getCanvasElementById
                       , getContext2D
                       , rotate
                       , setFillStyle
                       , setStrokeStyle
                       , strokePath
                       , translate
                       , withContext
                       )
import Graphics.Drawing.Color ( colorString
                              , rgb)

-- import Control.Monad.Eff.Console.Unsafe (logAny)

import Exercises1 (getScreenRect)
import CanvasPatch ( makeCanvasImageSource)

-- Exercise 1
strokeAndFillPath ctx path = do
  fillPath ctx path
  strokePath ctx path

-- Exercise 2
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

main = do
  Just canvas <- getCanvasElementById "canvas"
  ctx <- getContext2D canvas
  let screenRect = getScreenRect ctx
  node <- querySelector "#canvas"

  for node $ addEventListener "click" $ do
    withContext ctx do
      url <- canvasToDataURL canvas
      canvasImageSource <- makeCanvasImageSource url
      clearRect ctx screenRect
      translate { translateX: 0.5*screenRect.w
                , translateY: 0.5*screenRect.h } ctx
      rotate (0.01*Math.pi) ctx
      translate { translateX: (-0.5)*screenRect.w
                , translateY: (-0.5)*screenRect.h } ctx
      drawImage ctx canvasImageSource 0.0 0.0

    fillColor <- randomColor
    strokeColor <- randomColor
    circlePath <- randomCirclePath
    setFillStyle fillColor ctx
    setStrokeStyle strokeColor ctx
    strokeAndFillPath ctx $ arc ctx circlePath

    return unit
