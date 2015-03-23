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

-- Third set of exercises

instance eqNonEmpty :: (Eq a) => Eq (NonEmpty a) where
  (==) (NonEmpty x xs) (NonEmpty y ys) = (x == y) && (xs == ys)
  (/=) (NonEmpty x xs) (NonEmpty y ys) = (x /= y) || (xs /= ys)

data Extended a = Finite a | Infinite

instance showExtended :: (Show a) => Show (Extended a) where
  show (Finite x) = "Finite " ++ show x
  show Infinite = "∞"

equalsExtended :: forall a. (Eq a) => (Extended a) -> (Extended a) -> Boolean
equalsExtended (Finite x) (Finite y) = (==) x y
equalsExtended (Finite x) Infinite = false
equalsExtended Infinite (Finite y) = false
equalsExtended Infinite Infinite = true

notEqualsExtended :: forall a. (Eq a) => (Extended a) -> (Extended a) -> Boolean
notEqualsExtended e e' = not $ equalsExtended e e'

instance eqExtended :: (Eq a) => Eq (Extended a) where
  (==) e e' = equalsExtended e e'
  (/=) e e' = notEqualsExtended e e'

orderExtended :: forall a. (Ord a) => (Extended a) -> (Extended a) -> Ordering
orderExtended (Finite x) (Finite y) = compare x y
orderExtended (Finite x) Infinite = LT
orderExtended Infinite (Finite y) = GT
orderExtended Infinite Infinite = EQ

instance ordExtended :: (Ord a) => Ord (Extended a) where
  compare e e' = orderExtended e e'
