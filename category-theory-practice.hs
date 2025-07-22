{-# LANGUAGE Arrows #-}
{-# LANGUAGE InstanceSigs #-}

{-
Demonstrates Category Theory concepts in practical Haskell
- Functors, Applicative Functors, and Monads
- Natural Transformations
- Arrows and Arrow notation
- Kleisli categories
- Practical applications of abstract mathematics
-}

module CategoryTheoryPractice where

import Control.Arrow
import Control.Category
import Control.Monad
import Data.Functor.Identity
import Prelude hiding (id, (.))

-- Custom Functor: Binary Tree
data Tree a = Leaf a | Branch (Tree a) (Tree a)
  deriving (Show, Eq)

instance Functor Tree where
  fmap f (Leaf x) = Leaf (f x)
  fmap f (Branch left right) = Branch (fmap f left) (fmap f right)

-- Custom Applicative: Validation (accumulates errors)
data Validation e a = Failure e | Success a
  deriving (Show, Eq)

instance Functor (Validation e) where
  fmap _ (Failure e) = Failure e
  fmap f (Success a) = Success (f a)

instance Monoid e => Applicative (Validation e) where
  pure = Success
  
  Success f <*> Success a = Success (f a)
  Success _ <*> Failure e = Failure e
  Failure e <*> Success _ = Failure e
  Failure e1 <*> Failure e2 = Failure (e1 <> e2)

-- Example: Form validation
data User = User String Int String deriving Show

validateName :: String -> Validation [String] String
validateName name
  | length name >= 2 = Success name
  | otherwise = Failure ["Name must be at least 2 characters"]

validateAge :: Int -> Validation [String] Int
validateAge age
  | age >= 0 && age <= 150 = Success age
  | otherwise = Failure ["Age must be between 0 and 150"]

validateEmail :: String -> Validation [String] String
validateEmail email
  | '@' `elem` email = Success email
  | otherwise = Failure ["Email must contain @"]

validateUser :: String -> Int -> String -> Validation [String] User
validateUser name age email = User <$> validateName name <*> validateAge age <*> validateEmail email

-- Natural Transformations
maybeToList :: Maybe a -> [a]
maybeToList Nothing = []
maybeToList (Just x) = [x]

listToMaybe :: [a] -> Maybe a
listToMaybe [] = Nothing
listToMaybe (x:_) = Just x

-- Kleisli Category
(>=>) :: Monad m => (a -> m b) -> (b -> m c) -> (a -> m c)
f >=> g = \x -> f x >>= g

-- Example Kleisli arrows
safeDivide :: Double -> Double -> Maybe Double
safeDivide _ 0 = Nothing
safeDivide x y = Just (x / y)

safeLog :: Double -> Maybe Double
safeLog x
  | x > 0 = Just (log x)
  | otherwise = Nothing

safeSqrt :: Double -> Maybe Double
safeSqrt x
  | x >= 0 = Just (sqrt x)
  | otherwise = Nothing

-- Compose safe operations
safeOperation :: Double -> Double -> Maybe Double
safeOperation x y = safeDivide x y >=> safeLog >=> safeSqrt $ x

-- Comonads: Dual of Monads
class Functor w => Comonad w where
  extract :: w a -> a
  duplicate :: w a -> w (w a)
  extend :: (w a -> b) -> w a -> w b
  
  -- Default implementations
  duplicate = extend id
  extend f = fmap f . duplicate

-- Non-empty list Comonad
data NonEmpty a = a :| [a]
  deriving (Show, Eq)

instance Functor NonEmpty where
  fmap f (x :| xs) = f x :| fmap f xs

instance Comonad NonEmpty where
  extract (x :| _) = x
  
  duplicate list@(x :| xs) = list :| case xs of
    [] -> []
    (y:ys) -> [y :| ys]

-- Yoneda Lemma in practice
newtype Yoneda f a = Yoneda (forall b. (a -> b) -> f b)

-- Lower from Yoneda
lowerYoneda :: Yoneda f a -> f a
lowerYoneda (Yoneda y) = y id

-- Lift to Yoneda
liftYoneda :: Functor f => f a -> Yoneda f a
liftYoneda fa = Yoneda (\f -> fmap f fa)

-- Yoneda preserves functor operations with better performance
instance Functor (Yoneda f) where
  fmap f (Yoneda y) = Yoneda (\g -> y (g . f))

-- Practical example: Endo (Endomorphism)
newtype Endo a = Endo (a -> a)

instance Semigroup (Endo a) where
  Endo f <> Endo g = Endo (f . g)

instance Monoid (Endo a) where
  mempty = Endo id

-- Practical examples
exampleTree :: Tree Int
exampleTree = Branch (Leaf 1) (Branch (Leaf 2) (Leaf 3))

demonstrateFunctors :: IO ()
demonstrateFunctors = do
  putStrLn "Functor Examples:"
  putStrLn $ "Original tree: " ++ show exampleTree
  putStrLn $ "After (*2): " ++ show (fmap (*2) exampleTree)
  putStrLn $ "Functor composition: " ++ show (fmap ((+1) . (*2)) exampleTree)

demonstrateApplicative :: IO ()
demonstrateApplicative = do
  putStrLn "\nApplicative Validation Examples:"
  
  let user1 = validateUser "Alice" 25 "alice@example.com"
      user2 = validateUser "A" 200 "invalid-email"
      user3 = validateUser "" (-5) ""
  
  putStrLn $ "Valid user: " ++ show user1
  putStrLn $ "Invalid user (some errors): " ++ show user2
  putStrLn $ "Invalid user (all errors): " ++ show user3

demonstrateKleisli :: IO ()
demonstrateKleisli = do
  putStrLn "\nKleisli Composition Examples:"
  
  let result1 = safeOperation 16 4
      result2 = safeOperation 10 0
      result3 = safeOperation (-4) 2
  
  putStrLn $ "16 / 4 |> log |> sqrt: " ++ show result1
  putStrLn $ "10 / 0 |> log |> sqrt: " ++ show result2
  putStrLn $ "-4 / 2 |> log |> sqrt: " ++ show result3

demonstrateYoneda :: IO ()
demonstrateYoneda = do
  putStrLn "\nYoneda Lemma Examples:"
  
  let listExample = [1, 2, 3, 4, 5]
      yonedaList = liftYoneda listExample
      processed = fmap (*2) $ fmap (+1) yonedaList
      result = lowerYoneda processed
  
  putStrLn $ "Original list: " ++ show listExample
  putStrLn $ "After Yoneda processing: " ++ show result
  putStrLn "Yoneda allows fusion of fmap operations for better performance"

-- Main demonstration
main :: IO ()
main = do
  putStrLn "Category Theory in Practice"
  putStrLn "=========================="
  demonstrateFunctors
  demonstrateApplicative
  demonstrateKleisli
  demonstrateYoneda
  putStrLn "\nCategory theory provides the mathematical foundation for functional programming!"