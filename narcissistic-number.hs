{-
A Narcissistic Number is a number which is the sum of its own digits, each raised to the power of the number of digits.

For example, take 153 (3 digits):

    1^3 + 5^3 + 3^3 = 1 + 125 + 27 = 153
and 1634 (4 digits):

    1^4 + 6^4 + 3^4 + 4^4 = 1 + 1296 + 81 + 256 = 1634
The Challenge:

Your code must return true or false depending upon whether the given number is a Narcissistic number.

Error checking for text strings or other invalid inputs is not required, only valid integers will be passed into the function.

-}

module Narcissistic where

import Data.Char

narcissistic :: Integral a => a -> Bool
narcissistic n = n == fromIntegral (sum ((^ digitCount) <$> digits))
  where
    digits = digitToInt <$> (show $ fromIntegral n)
    digitCount = length digits

{-
narcissistic :: Integral n => n -> Bool
narcissistic n = n == fromIntegral (sum (map (^d) n'))
  where d = length n'
        n' = map digitToInt (show (fromIntegral n))

-}

{-
module Narcissistic where

digits n
  | n < 10 = [n]
  | otherwise = (n `mod` 10):(digits (n `div` 10))

narcissistic n = n == powsum
  where powsum = sum $ map (^ power) ds
        ds = digits n
        power = length ds

-}