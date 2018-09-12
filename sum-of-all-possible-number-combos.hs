{-
Jon and Joe have received equal marks in the school examination. But, they won't reconcile in peace when equated with each other. To prove his might, Jon challenges Joe to write a program to find all possible number combos that sum to a given number. While unsure whether he would be able to accomplish this feat or not, Joe accpets the challenge. Being Joe's friend, your task is to help him out.

Task
Create a function combos, that accepts a single positive integer num (30 > num > 0) and returns an array of arrays of positive integers that sum to num.

Notes
Sub-arrays may or may not have their elements sorted.
The order of sub-arrays inside the main array does not matter.
For an optimal solution, the following operation should complete within 6000ms.
Sample
    combos(3) => [ [ 3 ], [ 1, 1, 1 ], [ 1, 2 ] ]
    combos(10) => [ [ 10 ],
      [ 1, 1, 1, 1, 1, 1, 1, 1, 1, 1 ],
        [ 1, 1, 1, 1, 1, 1, 1, 1, 2 ],
        [ 1, 1, 1, 1, 1, 1, 1, 3 ],
        [ 1, 1, 1, 1, 1, 1, 4 ],
        [ 1, 1, 1, 1, 1, 5 ],
        [ 1, 1, 1, 1, 6 ],
        [ 1, 1, 1, 7 ],
        [ 1, 1, 8 ],
        [ 1, 9 ],
        [ 1, 1, 1, 1, 1, 1, 2, 2 ],
        [ 1, 1, 1, 1, 1, 2, 3 ],
        [ 1, 1, 1, 1, 2, 4 ],
        [ 1, 1, 1, 1, 2, 2, 2 ],
        [ 1, 1, 1, 1, 3, 3 ],
        [ 1, 1, 1, 2, 5 ],
        [ 1, 1, 1, 2, 2, 3 ],
        [ 1, 1, 1, 3, 4 ],
        [ 1, 1, 2, 6 ],
        [ 1, 1, 2, 2, 4 ],
        [ 1, 1, 2, 2, 2, 2 ],
        [ 1, 1, 2, 3, 3 ],
        [ 1, 1, 3, 5 ],
        [ 1, 1, 4, 4 ],
        [ 1, 2, 7 ],
        [ 1, 2, 2, 5 ],
        [ 1, 2, 2, 2, 3 ],
        [ 1, 2, 3, 4 ],
        [ 1, 3, 6 ],
        [ 1, 3, 3, 3 ],
        [ 1, 4, 5 ],
        [ 2, 8 ],
        [ 2, 2, 6 ],
        [ 2, 2, 2, 4 ],
        [ 2, 2, 2, 2, 2 ],
        [ 2, 2, 3, 3 ],
        [ 2, 3, 5 ],
        [ 2, 4, 4 ],
        [ 3, 7 ],
        [ 3, 3, 4 ],
        [ 4, 6 ],
        [ 5, 5 ] ]
-}

module Codewars.Kata.Combos where

combos :: Int -> [[Int]]
combos n =
  let
    f n s = foldl (\r -> \i -> (r ++) $ map (i :) (f (n - 1) i)) [[n]] [s..(div n 2)]
  in
    f n 1

{-
ps = [] : map combos [1..]
combos n = [n] : [x : p | x <- [1..n], p <- ps !! (n - x), x <= head p]

cache = [] : (map (combos) [1..])
combos n = [n] : [x:s | x <- [1..n-1], s <- cache !! (n-x), x >= head s]

import Data.List (nub, sort)

combos :: Int -> [[Int]]
combos n = go n n
  where go n m | n > 0 = [x : ps | x <- [1..min n m], ps <- go (n - x) x]
        go 0 _         = [[]]
        go _ _         = []

iter :: Int -> Int -> Int -> [Int] -> [[Int]] -> [[Int]]
iter d n c xs xxs
    | d == n = xs:xxs
    | otherwise = concat.map (\a -> iter d (a+n) a (a:xs) xxs) $ [c..d-n]

combos :: Int -> [[Int]]
combos n = iter n 0 1 [] []

combos :: Int -> [[Int]]
combos n = go n 1
  where go 0 _ = [[]]
        go n k = for [k..n] $ \i -> map (i:) $ go (n - i) i
        for = flip concatMap

-}
