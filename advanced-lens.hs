{-# LANGUAGE Rank2Types #-}

module Lens where

import Data.Functor.Identity  (Identity(..))
import Control.Applicative    (Const(..))
import Data.Function          ((&))
import Data.Traversable       (traverse)
import Data.Monoid            (First(..), Any(..))

{-
set _1 ((1,2),3) 3
set _1 ((1,2),3) True

how to make it work?
-}

type Lens s t a b = forall f . Functor f => (a -> f b) -> s -> f t

type Lens' s a = Lens s s a a
-- Or
-- {-# LANGUAGE LiberalTypeSynonyms #-}
-- type Simple f a b = f a a b b
-- type Lens' = Simple Lens

lens :: (s -> a) -> (s -> b -> t) -> Lens s t a b
lens getter setter f s = setter s <$> f (getter s)

type Getting r s a = (a -> Const r a) -> s -> Const r s
type Setter s t a b = (a -> Identity b) -> s -> Identity t
type Setting a b = Setter a a b b
-- type Setting a b = Simple Setter

over :: Setter s t a b -> (a -> b) -> s -> t
over lens f = runIdentity . lens (Identity . f)

set :: Setter s t a b -> b -> s -> t
set lens a = over lens (const a)

view :: Getting a s a -> s -> a
view lens = getConst . lens Const

_1 :: Functor f => (t -> f a) -> (t, b) -> f (a, b)
_1 f (a, b) = fmap (\c -> (c, b)) $ f a
-- _1 f s = fmap (\c -> (c, snd s)) $ f (fst s)

_length :: Lens' [a] Int
_length f l = const l <$> f (length l)

_abs :: (Num a, Ord a) => Lens' a a
_abs f i = setabs <$> f (abs i)
  where
    sign x
      | x >= 0 = 1
      | x < 0 = -1
    setabs x
      | x >= 0 = x * sign i
      | x < 0 = error "Abs can't be negative"

-- view traverse ['1', '2', '3']
-- error: No instance for (Monoid Char) arising from a use of ‘traverse’

toListOf :: Getting [a] s a -> s -> [a]
toListOf lens = getConst . lens (\a -> Const [a])

_all :: (Traversable t, Applicative f) => (a -> Bool) -> (a -> f a) -> t a -> f (t a)
_all f t s = traverse (\a -> if f a then t a else pure a) s

{-
newtype First a = First { getFirst :: Maybe a }
        deriving (Eq, Ord, Read, Show, Generic, Generic1,
                  Functor, Applicative, Monad)

instance Monoid (First a) where
        mempty = First Nothing
        First Nothing `mappend` r = r
        l `mappend` _             = l

Defined in 'Data.Monoid'
-}

preview :: Getting (First a) s a -> s -> Maybe a
preview lens = getFirst . getConst . lens (Const . First . Just)

has :: Getting Any s a -> s -> Bool
has lens = getAny . getConst . lens (const $ Const (Any True))

main :: IO ()
main = do
  putStrLn $ "origin value: " ++ show ((1, 2), 3)
  putStrLn $ "normal over: " ++ show (over _1 (fmap (+1)) ((1, 2), 3))
  putStrLn $ "normal view: " ++ show (view _1 ((1, 2), 3))
  putStrLn $ "normal set: " ++ show (set _1 (2, 3) ((1, 2), 3))
  putStrLn $ "over with another type: " ++ show (over _1 Just ((1, 2), 3))
  putStrLn $ "set with another type: " ++ show (set _1 True ((1, 2), 3))
  putStrLn $ show (view (_1 . _length) ("hello", 3))
  putStrLn $ show (set (_1 . _length) 9 ("world", 3))
  putStrLn $ show (view _abs (-123))
  putStrLn $ show (set _abs 13 (-99))
  putStrLn $ show (over traverse (+1) [1, 2, 3])
  putStrLn $ show (view traverse ["1", "2", "3"])
  putStrLn $ show (toListOf traverse ['1', '2', '3'])
  putStrLn $ show (toListOf (_all (/= 0)) [1, 2, 0, 3, 4, 0, 5])
  putStrLn $ show (toListOf (traverse . traverse) [[1, 2], [3, 4], [5]])
  putStrLn $ show (over (_all (\x -> length x <= 3) . traverse) (+1) [[1, 2], [1, 2, 3, 4], [4, 5, 6], [23, 4, 5, 5, 4], [1], [2, 3]])
  -- [[2,3],[1,2,3,4],[5,6,7],[23,4,5,5,4],[2],[3,4]]
  putStrLn $ show (toListOf (traverse . _1) [(1, 2), (3, 4), (5, 6)])
  -- [1,3,5]
  putStrLn $ show (preview (_all (/= 0)) [3, 2, 1, 0])
  -- Just 3
  putStrLn $ show (preview (_all (/= 0)) [0, 0, 0])
  -- Nothing
  putStrLn $ show (has (_all (==0)) [3, 2, 1, 0])
  -- True

{-
see https://zhuanlan.zhihu.com/p/31328798

-}
