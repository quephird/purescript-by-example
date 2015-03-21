module Chapter4 where

import Control.MonadPlus (guard)
import Data.Array ((..), concatMap, filter, length, map, null)
import Data.Array.Unsafe (head, tail)
import Data.Foldable (foldl, foldr)
import Data.Maybe.Unsafe (fromJust)

import Data.Path

-- First set of exercises

even :: Number -> Boolean
even n =
  if n < 0
  then even (-n)
  else if n == 0
       then true
       else if n == 1
            then false
            else even (n-2)

countEven :: [Number] -> Number
countEven xs =
  if null xs
  then 0
  else if even $ head xs
       then (+1) $ countEven $ tail xs
       else countEven $ tail xs

-- Second set of exercises

squareAll :: [Number] -> [Number]
squareAll = map (\n -> n*n)

removeNegatives :: [Number] -> [Number]
removeNegatives = filter (\n -> n>=0)

(<$?>) :: forall a. (a -> Prim.Boolean) -> [a] -> [a]
(<$?>) = filter

-- Third set of exercises

factors :: Number -> [[Number]]
factors n = do
  i <- 1 .. n
  j <- i .. n
  guard $ i * j == n
  return [i, j]

isPrime :: Number -> Boolean
isPrime = (==1) <<< length <<< factors

cartProd :: [Number] -> [Number] -> [[Number]]
cartProd xs ys = do
  x <- xs
  y <- ys
  return [x, y]

triples :: Number -> [[Number]]
triples n = do
  x <- 3 .. n
  y <- x .. n
  z <- y .. n
  guard $ x*x + y*y == z*z
  return [x, y, z]

-- Fourth set of exercises

allTrue :: [Boolean] -> Boolean
allTrue = foldl (\x acc -> x && acc) true

count :: forall a. (a -> Boolean) -> [a] -> Number
count p xs = count' p xs 0 where
  count' _ [] acc     = acc
  count' p (x:xs) acc = if p x
                        then count' p xs (acc+1)
                        else count' p xs acc

reverse :: forall a. [a] -> [a]
reverse = foldl (\xs x -> [x] ++ xs) []

-- Fifth set of exercises

allFiles :: Path -> [Path]
allFiles file = file : concatMap allFiles (ls file)

onlyFiles :: Path -> [Path]
onlyFiles = filter isFile <<< allFiles where
  isFile = not <<< isDirectory

selectFileWith :: (Number -> Number -> Prim.Boolean) -> Path
selectFileWith comparator = foldl fileCompare firstFile $ restFiles where
  firstFile = head $ onlyFiles root
  restFiles = tail $ onlyFiles root
  size' = fromJust <<< size
  fileCompare x acc = if comparator (size' x) (size' acc)
                      then x
                      else acc

largestFile :: Path
largestFile = selectFileWith (>)

smallestFile :: Path
smallestFile = selectFileWith (<)
