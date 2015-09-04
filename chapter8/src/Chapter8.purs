module Chapter8 where

import Prelude (($), (==), (/=), (<=), (+), (*), (/), bind, pure, return, unit)

import Control.Monad.Eff
import Control.Monad.Eff.Exception
import Control.Monad.Eff.Random
import Control.Monad.ST
import Data.Array ((!!), (:), filter, head, length, tail)
import Data.Int (round, toNumber)
import Data.Maybe
import Data.Maybe.Unsafe (fromJust)
import Data.Tuple (Tuple(..))
import Debug.Trace

-- First set

third :: forall a. Array a -> Maybe a
third xs = do
  xs' <- tail xs
  xs'' <- tail xs'
  f <- head xs''
  return f

-- Second set

safeDivide :: Int -> Int -> Maybe Int
safeDivide _ 0 = Nothing
safeDivide a b = Just (a / b)

safeDivide' :: forall eff. Int -> Int -> Eff (err :: EXCEPTION | eff) Int
safeDivide' _ 0 = throwException (error "Denominator does not divide numerator")
safeDivide' a b = pure (a / b)

pi :: forall eff r. Int -> Eff (st :: ST r, random :: RANDOM | eff) Number
pi n = do
  ref <- newSTRef ([] :: Array (Tuple Number Number))
  forE 0.0 (toNumber n) $ \i -> do
    x <- random
    y <- random
    modifySTRef ref (\o -> Tuple x y : o)
    return unit
  tuples <- readSTRef ref
  inside <- return $ toNumber $ length $ filter (\(Tuple x y) -> x*x + y*y <= 1.0) tuples
  return (4.0 * inside / toNumber n)
