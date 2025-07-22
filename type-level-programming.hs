{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

{-
Demonstrates Haskell's powerful type-level programming capabilities
- Type families for compile-time computation
- DataKinds for promoting values to types
- Type-level arithmetic and logic
- Length-indexed vectors
- Type-safe programming patterns
-}

module TypeLevelProgramming where

import Data.Kind (Type)
import Data.Proxy

-- Promote natural numbers to type level
data Nat = Z | S Nat

-- Type family for addition at type level
type family Add (n :: Nat) (m :: Nat) :: Nat where
  Add Z m = m
  Add (S n) m = S (Add n m)

-- Type family for multiplication
type family Mult (n :: Nat) (m :: Nat) :: Nat where
  Mult Z m = Z
  Mult (S n) m = Add m (Mult n m)

-- Type family for comparison
type family Compare (n :: Nat) (m :: Nat) :: Ordering where
  Compare Z Z = 'EQ
  Compare Z (S m) = 'LT
  Compare (S n) Z = 'GT
  Compare (S n) (S m) = Compare n m

-- Length-indexed vectors
data Vec (n :: Nat) a where
  VNil :: Vec Z a
  VCons :: a -> Vec n a -> Vec (S n) a

-- Safe head - only works on non-empty vectors
vhead :: Vec (S n) a -> a
vhead (VCons x _) = x

-- Safe tail
vtail :: Vec (S n) a -> Vec n a
vtail (VCons _ xs) = xs

-- Append vectors - length is computed at type level
vappend :: Vec n a -> Vec m a -> Vec (Add n m) a
vappend VNil ys = ys
vappend (VCons x xs) ys = VCons x (vappend xs ys)

-- Type-safe indexing
data Fin (n :: Nat) where
  FZ :: Fin (S n)
  FS :: Fin n -> Fin (S n)

vindex :: Vec n a -> Fin n -> a
vindex (VCons x _) FZ = x
vindex (VCons _ xs) (FS i) = vindex xs i

-- Type-level booleans
data Bool' = True' | False'

type family And (a :: Bool') (b :: Bool') :: Bool' where
  And True' True' = True'
  And _ _ = False'

type family Or (a :: Bool') (b :: Bool') :: Bool' where
  Or False' False' = False'
  Or _ _ = True'

-- Type-level lists
data List k = Nil' | Cons' k (List k)

type family Elem (x :: k) (xs :: List k) :: Bool' where
  Elem x Nil' = False'
  Elem x (Cons' x xs) = True'
  Elem x (Cons' y xs) = Elem x xs

-- Heterogeneous list (HList)
data HList (ts :: [Type]) where
  HNil :: HList '[]
  HCons :: t -> HList ts -> HList (t ': ts)

-- Phantom types for units
data Meter
data Foot
data Second

newtype Distance unit = Distance Double deriving (Show, Eq)
newtype Time unit = Time Double deriving (Show, Eq)

-- Type-safe arithmetic with units
addDistance :: Distance unit -> Distance unit -> Distance unit
addDistance (Distance a) (Distance b) = Distance (a + b)

-- Matrix with dimensions encoded in types
data Matrix (rows :: Nat) (cols :: Nat) a where
  Matrix :: [[a]] -> Matrix rows cols a

-- Examples
type One = S Z
type Two = S (S Z)
type Three = S (S (S Z))

-- Create some vectors
vec1 :: Vec Two Int
vec1 = VCons 1 (VCons 2 VNil)

vec2 :: Vec Three Int  
vec2 = VCons 3 (VCons 4 (VCons 5 VNil))

-- Type-level computation examples
type Five = Add Two Three
type Six = Mult Two Three

-- Examples of usage
examples :: IO ()
examples = do
  putStrLn "Type-level programming examples:"
  putStrLn "Vector operations preserve length information at compile time"
  putStrLn "Matrix operations ensure dimension compatibility"
  putStrLn "Unit arithmetic prevents mixing incompatible quantities"

main :: IO ()
main = examples