module Chapter2 where

import Debug.Trace
import Math

circleArea r = Math.pi * r * r

diagonal w h = sqrt (w * w + h * h)

main = print (diagonal 3 4)
