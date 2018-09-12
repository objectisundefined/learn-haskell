{-
Task
You are given a string s. Every letter in s appears once.

Consider all strings formed by rearranging the letters in s. After ordering these strings in dictionary order, return the middle term. (If the sequence has a even length n, define its middle term to be the (n/2)th term.)

Example
For s = "abc", the result should be "bac".

The permutations in order are:
"abc", "acb", "bac", "bca", "cab", "cba"
So, The middle term is "bac".
Input/Output
[input] string s

unique letters (2 <= length <= 26)

[output] a string

middle permutation.

-}

module MiddlePermutation.JorgeVS.Kata where

import Data.List (sort)

middlePermutation :: String -> String
middlePermutation str = 
  let
    r = permutation str
  in
    (sort r) !! ((length r `div` 2) - 1)

permutation :: String -> [String]
permutation str =
  case str of
    [] -> []
    [x] -> [[x]]
    x : xs ->
      let
        r = permutation xs
        l = length xs
        f v = map (\i -> (take i v) ++ [x] ++ (drop i v)) [0..l]
      in
        concat $ map f r

{-
import Data.List

middlePermutation :: String -> String
middlePermutation myString  = (sort strs)!!(((length strs) `div` 2) - 1)
                  where strs = permutations myString

-}

{-
import Data.List (permutations)

permutations :: [a] -> [[a]]

-- >>> permutations "abc"
-- ["abc","bac","cba","bca","cab","acb"]
-}
