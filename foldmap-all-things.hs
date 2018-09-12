{-
The foldMap function can be used to implement all kind of folds. Here is its signature :

foldMap :: (Monoid m, Foldable t) => (a -> m) -> t a -> m
In this exercise, you will have to implement the following functions, by increasing difficulty, in terms of foldMap :

myToList : turn any Foldable into a list
myMinimum : get the minimum value from any Foldable (hint : you will have to write a custom type, with a custom Monoid instance)
myFoldr : implement foldr in terms of foldMap (hint : there is a suitable Monoid in Data.Monoid)
There should be a single use of foldMap in each of the requested functions !

-}

module Foldmap where

import Data.Foldable (foldMap, Foldable)
import Data.Monoid

myToList :: Foldable t => t a -> [a]
myToList t = foldMap (\a -> [a]) t

newtype Min a = Min { getMin :: Maybe a }

instance Ord a => Monoid (Min a) where
  mempty = Min Nothing

  m `mappend` Min Nothing = m
  Min Nothing `mappend` n = n
  Min m@(Just x) `mappend` (Min n@(Just y))
    | x <= y = Min m
    | otherwise = Min n

myMinimum :: (Ord a, Foldable t) => t a -> Maybe a
myMinimum = getMin . foldMap (Min . Just)

myFoldr :: Foldable t => (a -> b -> b) -> b -> t a -> b
myFoldr f b t = appEndo (foldMap (Endo . f) t) b

{-
https://hackage.haskell.org/package/base-4.9.1.0/docs/src/Data.Foldable.html

-}

-- {-# OPTIONS_GHC -Wall -Werror #-}
{-
module Foldmap where

import Data.Monoid (Endo(Endo,appEndo))
import Data.Semigroup (Option(Option,getOption), Min(Min,getMin))

myToList :: Foldable t => t a -> [a]
myToList ta = foldMap (Endo . (:)) ta `appEndo` []

myMinimum :: (Ord a, Foldable t) => t a -> Maybe a
myMinimum = fmap getMin . getOption . foldMap (Option . Just . Min)

myFoldr :: Foldable t => (a -> b -> b) -> b -> t a -> b
myFoldr f b ta = foldMap (Endo . f) ta `appEndo` b

-}
