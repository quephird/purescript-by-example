module Chapter6 where

import Data.Array (map)
import Data.Foldable (Foldable, foldl, foldMap, foldr)
import Data.Picture

-- First set of exercises

instance showShape :: Show Shape where
  show s = showShape s

-- Second set of exercises

newtype Complex = Complex {
  real :: Number,
  imaginary :: Number
}

instance showComplex :: Show Complex where
  show (Complex {real = r, imaginary = i}) = show r ++ " + " ++ show i ++ "i"

instance eqComplex :: Eq Complex where
  (==) (Complex {real = r, imaginary = i}) (Complex {real = r', imaginary = i'}) =
    (r == r') && (i == i')
  (/=) (Complex {real = r, imaginary = i}) (Complex {real = r', imaginary = i'}) =
    (r /= r') || (i /= i')

-- This represents an array with at least one element.
data NonEmpty a = NonEmpty a [a]

instance showNonEmpty :: (Show a) => Show (NonEmpty a) where
  show (NonEmpty x xs) = show (x:xs)

instance semigroupNonEmpty :: Semigroup (NonEmpty a) where
  (<>) (NonEmpty x xs) (NonEmpty y ys) = (NonEmpty x (xs ++ (y:ys)))

instance functorNonEmpty :: Functor NonEmpty where
  (<$>) f (NonEmpty x xs) = (NonEmpty (f x) (map f xs))

instance foldableNonEmpty :: Foldable NonEmpty where
  foldr f a (NonEmpty x xs) = foldr f a (x:xs)
  foldl f a (NonEmpty x xs) = foldl f a (x:xs)
  foldMap f (NonEmpty x xs) = foldMap f (x:xs)
