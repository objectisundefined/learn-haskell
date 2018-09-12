{-
You have a positive number n consisting of digits. You can do at most one operation: Choosing the index of a digit in the number, remove this digit at that index and insert it back to another place in the number.

Doing so, find the smallest number you can get.

#Task: Return an array or a tuple or a string depending on the language (see "Sample Tests") with

1) the smallest number you got
2) the index i of the digit d you took, i as small as possible
3) the index j (as small as possible) where you insert this digit d to have the smallest number.
Example:

smallest(261235) --> [126235, 2, 0] or (126235, 2, 0) or "126235, 2, 0"
126235 is the smallest number gotten by taking 1 at index 2 and putting it at index 0

smallest(209917) --> [29917, 0, 1] or ...

[29917, 1, 0] could be a solution too but index `i` in [29917, 1, 0] is greater than 
index `i` in [29917, 0, 1].
29917 is the smallest number gotten by taking 2 at index 0 and putting it at index 1 which gave 029917 which is the number 29917.

smallest(1000000) --> [1, 0, 6] or ...
Note
Have a look at "Sample Tests" to see the input and output in each language

tests:
261235 -> (126235, 2, 0)
209917 -> (29917, 0, 1)
285365 -> (238565, 3, 1)
269045 -> (26945, 3, 0)
296837 -> (239687, 4, 1)
-}

module Codewars.G964.Tosmallest where

smallest :: Integer -> (Integer, Int, Int)
smallest n = minimum [ (read . f i j . show $ n , i, j) | i <- is, j <- is]
    where is = [0 .. pred . length . show $ n]
          f i j xs = let (e, l) = g i xs in h j e l
          g i xs = (xs !! i, take i xs ++ drop (i + 1) xs)
          h i e xs = take i xs ++ [e] ++ drop i xs

-- loop O(n^2)
{-
module Codewars.G964.Tosmallest where

import Data.List

smallest :: Integer -> (Integer, Int, Int)
smallest n = head $ sort [(action i j, i, j) | i <- [0..l], j <- [0..l]]
    where nprinted = show n
          l = length nprinted - 1
          digit i = nprinted !! i
          wodigit i = (take i nprinted) ++ (drop (i + 1) nprinted)
          action i j = let v = wodigit i in read $ (take j v) ++ [digit i] ++ (drop j v)

-}

{-
read :: Read a => String -> a
read "123" :: Int -- 123

pred :: Enum a => a -> a
-- the predecessor of a value. For numeric types, pred subtracts 1.

-}
