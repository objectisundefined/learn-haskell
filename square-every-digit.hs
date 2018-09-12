{-
  For example, if we run 9119 through the function,
  811181 will come out, because 92 is 81 and 12 is 1.
-}

module SquareDigit where

import Data.Char (digitToInt)

squareDigit :: Int -> Int
squareDigit n | n >= 0 = (dightsToInt . concatMap (show . square . digitToInt) . show) n
              | otherwise = (negate . squareDigit . negate) n

square :: Num a => a -> a
square x = x * x

dightsToInt :: [Char] -> Int
dightsToInt = foldl (\a x -> a * 10 + digitToInt x) 0

{-
import Data.Char

squareDigit :: Int -> Int
squareDigit n = neg . read . concatMap ( show . (^2) . digitToInt ) . show . abs $ n
  where neg = if n < 0 then negate else id

-}

{-
import Data.Char
import Data.Maybe

squareChar :: Char -> Maybe Int
squareChar '-' = Nothing
squareChar x = let y = digitToInt x in Just (y * y)

concatSq :: String -> Maybe Int -> String
concatSq "" Nothing = "-"
concatSq s (Just i) = s ++ show i

squareDigit :: Int -> Int
squareDigit = read . foldl concatSq "" . map squareChar . show

-}

{-
import Data.Char

squareDigit :: Int -> Int
squareDigit n
  | n < 0 = negate (squareDigit (negate n))
  | otherwise = read (show n >>= (show . (^2) . digitToInt))

-}

{-
import Data.Char
import Control.Applicative

squareDigit :: Int -> Int
squareDigit = (*) . signum <*> read . concatMap (show . (^(2 :: Int)) . digitToInt) . show . abs

-}

{-
abs :: a -> a
-- Absolute value.

signum :: a -> a Source#
  -- Sign of a number. The functions abs and signum should satisfy the law:

  -- abs x * signum x == x
  -- For real numbers, the signum is either -1 (negative), 0 (zero) or 1 (positive).

instance Applicative ((->) a) where
  pure = const
  (<*>) f g x = f x (g x)

-}

{-
import Data.Char

squareDigit :: Int -> Int
squareDigit = f >>= g
  where
    f = read . concatMap (show . (^(2 :: Int)) . digitToInt) . show . abs
    g = flip ((*) . signum)

instance Monad ((->) r) where
  return = const
  f >>= k = \ r -> k (f r) r

-}