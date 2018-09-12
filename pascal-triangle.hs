{-
# Pascal's Triangle

Pascal's Triangle

       1
      1 1
     1 2 1
    1 3 3 1
   1 4 6 4 1
   ....

Wikipedia article on Pascal's Triangle: http://en.wikipedia.org/wiki/Pascal's_triangle

Write a function that, given a depth (n), returns a single-dimensional array representing Pascal's Triangle to the n-th level.

For example:

pascalsTriangle 4 == [1,1,1,1,2,1,1,3,3,1]

-}

module Codewars.Kata.PascalsTriangle where
      
pascalsTriangle :: Int -> [Int]
pascalsTriangle n =
  if n == 1
  then [1]
  else
    let
      r = pascalsTriangle (n - 1)
      l = drop (length r + 1 - n) r
    in
      r ++ (zipWith (+) (l ++ [0]) ([0] ++ l))

{-
pascalsTriangle :: Int -> [Int]
pascalsTriangle n = concat $ take n $ iterate nextRow [1]
    where nextRow row = zipWith (+) ([0] ++ row) (row ++ [0])

-}

{-
-- Data.List (iterate)

iterate :: (a -> a) -> a -> [a] Source#

iterate f x returns an infinite list of repeated applications of f to x:

iterate f x == [x, f x, f (f x), ...]

-}
