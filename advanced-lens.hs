{-# LANGUAGE Rank2Types #-}

module Lens where

import Data.Functor.Identity  (Identity(..))
import Control.Applicative    (Const(..))
import Data.Function          ((&))

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
view lens = getConst . (lens Const)

type FirstL t a b = Lens (a, b) t a b

_1 :: Functor f => (t -> f a) -> (t, b) -> f (a, b)
_1 f (a, b) = fmap (\c -> (c, b)) $ f a
-- _1 f s = fmap (\c -> (c, snd s)) $ f (fst s)

_length :: Lens' [a] Int
_length f l = const l <$> f (length l)

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
