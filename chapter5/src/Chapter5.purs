module Chapter5 where

import Data.Array (map)
import Data.Maybe
import Data.Picture

-- First set of exercises

factorial :: Number -> Number
factorial 0 = 1
factorial n = n * factorial (n-1)

binomialCoefficient :: Number -> Number -> Number
binomialCoefficient n k | k > n = 0
binomialCoefficient n 0 = 1
binomialCoefficient n k = binomialCoefficient (n-1) (k-1) +
                          binomialCoefficient (n-1) k

-- Second set of exercises

allTrue :: [Boolean] -> Boolean
allTrue [] = true
allTrue (true : xs) = allTrue xs
allTrue _ = false

isSorted :: [Number] -> Boolean
isSorted [] = true
isSorted [_] = true
isSorted (x1 : x2 : xs) | x2 < x1 = false
isSorted (_ : xs) = isSorted xs

-- Third set of exercises

type Address = { street :: String,
                 city :: String,
                 state :: String,
                 zip :: String}
type Person = { first :: String,
                last :: String,
                address :: Address }

getCity :: Person -> String
getCity { address = { city = c } } = c

flatten :: forall a. [[a]] -> [a]
flatten [] = []
flatten (x : xs) = x ++ (flatten xs)

-- Fourth set of exercises

origin = Point {x: 0, y: 0}
circle = Circle origin 10

transformAll :: [Shape] -> [Shape]
transformAll = map transform where
  transform (Circle _ r) = Circle origin (2*r)
  transform (Rectangle _ w h) = Rectangle origin (2*w) (2*h)
  transform s = s

getText :: Shape -> Maybe String
getText (Text _ s) = Just s
getText _ = Nothing

-- Fourth set of exercises

area :: Shape -> Number
area (Circle _ r) = Math.pi * r * r
area (Rectangle _ w h) = w * h
area _ = 0
