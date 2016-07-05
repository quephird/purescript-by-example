module Exercises where

import Prelude ((*), (+))

import Control.Monad.Eff.Console (infoShow)
import Math (pi, sqrt)

circleArea r = pi * r * r

diagonal w h = sqrt (w * w + h * h)

main = infoShow (diagonal 3.0 4.0)
