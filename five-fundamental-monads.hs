{-
In this kata we will implement five of the most fundamental monads.

Newcomers to Haskell find monads to be one of the most intimidating concepts but on a basic level - they are not too difficult to understand.

A datatype forms a monad if it is possible to complete the following definitions such that the monad laws (described below) hold. There's nothing more to it! For a more intuitive understanding then there are a plethora of tutorials which use (sometimes wild) analogies to explain this concept.

class Monad m where
    return :: a -> m a
    (>>=) :: m a -> (a -> m b) -> m b
Monad laws

return x >>= f = f x
m >>= return = m
(m >>= f) >>= g = m >>= (\x -> f x >>= g)
It turns out that many different types of computation can be encapsulated by monads. For example the Maybe monad encapsulates a computation which can fail and State a computation with mutable state.

The five we will implement here are Identity, Maybe, State, Writer and Reader.

Hint: https://www.haskell.org/haskellwiki/Monad_tutorials_timeline

Note: Please feel free to contribute!

-}

{-# LANGUAGE NoImplicitPrelude #-}
module Monads where

import Prelude hiding (Monad, Identity, Maybe(..), State, Reader, Writer)
import Data.Monoid

class Monad m where
  return :: a -> m a
  (>>=) :: m a -> (a -> m b) -> m b

data Identity a = Identity a
  deriving (Show, Eq)

data Maybe a = Nothing | Just a
  deriving (Show, Eq)

data State s a = State {runState :: s -> (a, s)}

data Reader s a = Reader {runReader :: s -> a }

data Writer w a = Writer {runWriter :: (a, w)}

instance Monad Identity where
  return = Identity
  (Identity v) >>= f = f v

instance Monad Maybe where
  return = Just
  Nothing >>= f = Nothing
  (Just v) >>= f = f v

instance Monad (State s) where
  return a = State $ \s -> (a, s)
  (State g) >>= f = State $ \s ->
      let (a, s') = g s
      in runState (f a) s'

instance Monad (Reader s) where
  return a = Reader $ \_ -> a
  (Reader g) >>= f = Reader $ \s -> runReader (f $ g s) s

instance Monoid w => Monad (Writer w) where
  return x = Writer (x, mempty)
  (Writer (x,v)) >>= f =
    let (Writer (y, v')) = f x
    in Writer (y, v `mappend` v')

{-
http://www.learnyouahaskell.com/for-a-few-monads-more

-}
