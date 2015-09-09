module Chapter8 where

import Prelude (Monad, ($), (==), (/=), (<=), (+), (*), (/), bind, pure, return, unit)

import Control.Monad.Eff
import Control.Monad.Eff.Exception
import Control.Monad.Eff.Random
import Control.Monad.ST
import Data.Array ((!!), (:), filter, foldM, head, length, nub, sort, tail)
import Data.Int (round, toNumber)
import Data.List (List(..))
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

sums :: Array Int -> Array Int
sums lst = nub $ sort $ (foldM (\a e -> [a, a+e]) 0) lst

filterM' :: forall m a. (Monad m) => (a -> m Boolean) -> List a -> List a -> m (List a)
filterM' p acc Nil = return acc
filterM' p acc (Cons x xs) = do
  keep <- p x
  case keep of
    true  -> filterM' p (Cons x acc) xs
    false -> filterM' p acc xs

filterM :: forall m a. (Monad m) => (a -> m Boolean) -> List a -> m (List a)
filterM p lst = filterM' p Nil lst

-- Second set

safeDivide :: Int -> Int -> Maybe Int
safeDivide _ 0 = Nothing
safeDivide a b = Just (a / b)

safeDivide' :: forall eff. Int -> Int -> Eff (err :: EXCEPTION | eff) Int
safeDivide' _ 0 = throwException (error "Denominator does not divide numerator")
safeDivide' a b = pure (a / b)

pi :: forall r. Int -> Eff (st :: ST r, random :: RANDOM) Number
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
