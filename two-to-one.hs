{-
  Take 2 strings s1 and s2 including only letters from ato z. Return a new sorted string, the longest possible, containing distinct letters,

  each taken only once - coming from s1 or s2. #Examples: ``` a = "xyaabbbccccdefww" b = "xxxxyyyyabklmopq" longest(a, b) -> "abcdefklmopqwxy"
  a = "abcdefghijklmnopqrstuvwxyz" longest(a, a) -> "abcdefghijklmnopqrstuvwxyz" ```
-}

module Codewars.G964.Longest where

longest :: [Char] -> [Char] -> [Char]
longest s1 s2 = [c | c <- ['a'..'z'], c `elem` s1 || c `elem` s2]

{-
import Data.List (nub, sort)

longest :: [Char] -> [Char] -> [Char]
longest s1 s2 = nub $ sort $ s1 ++ s2
-}

{-
Data.List (nub)

O(n^2). The nub function removes duplicate elements from a list.
In particular, it keeps only the first occurrence of each element.
(The name nub means `essence'.) It is a special case of nubBy,
which allows the programmer to supply their own equality test. 
-}

{-
longest :: [Char] -> [Char] -> [Char]
longest s1 s2 = sort $ map head $ group $ s1 ++ s2
-}
