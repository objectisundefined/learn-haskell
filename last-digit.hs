{-
Define a function

lastDigit :: Integer -> Integer -> Integer
that takes in two numbers a and b and returns the last decimal digit of a^b. Note that a and b may be very large!

For example, the last decimal digit of 9^7 is 9, since 9^7 = 4782969. The last decimal digit of (2^200)^(2^300), which has over 10^92 decimal digits, is 6.

The inputs to your function will always be non-negative integers.

Examples
lastDigit 4 1             `shouldBe` 4
lastDigit 4 2             `shouldBe` 6
lastDigit 9 7             `shouldBe` 9
lastDigit 10 (10^10)      `shouldBe` 0
lastDigit (2^200) (2^300) `shouldBe` 6
Remarks
JavaScript
Since JavaScript doesn't have native arbitrary large integers, your arguments are going to be strings representing non-negative integers, e.g.

lastDigit("10", "10000000000");
The kata is still as hard as the variants for Haskell or Python, don't worry.

-}

module LastDigit where

nums :: [[Integer]]
nums = 
  [
    [0],
    [1],
    [2, 4, 8, 6],
    [3, 9, 7, 1],
    [4, 6],
    [5],
    [6],
    [7, 9, 3, 1],
    [8, 4, 2, 6],
    [9, 1]
  ]

lastDigit :: Integer -> Integer -> Integer
lastDigit a b | b == 0 = 1
              | otherwise =
                let
                  a' = a `mod` 10
                  l = nums !! (fromInteger a')
                  b' = (fromInteger b) `mod` (length l)
                in
                  if b' >= 1 then l !! (b' - 1) else last l

-- 0
-- 1
-- 2 4 8 6
-- 3 9 7 1
-- 4 6
-- 5
-- 6
-- 7 9 3 1
-- 8 4 2 6
-- 9 1

{-
lastDigit :: Integer -> Integer -> Integer
lastDigit a b = ((a `rem` 10) ^ ((b - 1) `rem` 4 + 1)) `rem` 10

-- fill every elem in nums to length 4
-- eg: 5 -> [5, 5, 5, 5]

-}

{-
rem :: Integral => a -> a -> a

integer remainder, satisfying
(x `quot` y)*y + (x `rem` y) == x

mod :: Integral => a -> a -> a

nteger modulus, satisfying
(x `div` y)*y + (x `mod` y) == x

quot :: a -> a -> a

integer division truncated toward zero
(-10) `quot` 4 -> -2
10 `quot` 4 -> 2

div :: a - a -> a

integer division truncated toward negative infinity
(-10) `div` 4 -> -3
10 `div` 4 -> 2
-}
