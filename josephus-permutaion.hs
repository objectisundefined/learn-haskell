{-
Tips and notes: it helps to start counting from 1 up to n, instead of the usual range 0..n-1; k will always be >=1.

For example, with n=7 and k=3 josephus(7,3) should act this way.

[1,2,3,4,5,6,7] - initial sequence
[1,2,4,5,6,7] => 3 is counted out and goes into the result [3]
[1,2,4,5,7] => 6 is counted out and goes into the result [3,6]
[1,4,5,7] => 2 is counted out and goes into the result [3,6,2]
[1,4,5] => 7 is counted out and goes into the result [3,6,2,7]
[1,4] => 5 is counted out and goes into the result [3,6,2,7,5]
[4] => 1 is counted out and goes into the result [3,6,2,7,5,1]
[] => 4 is counted out and goes into the result [3,6,2,7,5,1,4]
So our final result is:

josephus([1,2,3,4,5,6,7],3)==[3,6,2,7,5,1,4]
For more info, browse the Josephus Permutation page on wikipedia; related kata: Josephus Survivor.

-}

module Josephus where

josephus :: [a] -> Int -> [a]
josephus [] _ = []
josephus xs k = let n = (k-1) `mod` length xs in (xs!!n) : josephus (drop (n+1) xs ++ take n xs) k

{-
josephus :: [a] -> Int -> [a]
josephus [] _ = []
josephus ls k = let (x:xs) = drop (k - 1) $ cycle ls in
                x : take (length ls - 1) xs `josephus` k

-}

{-
josephus :: [a] -> Int -> [a]
josephus [] _ = []
josephus xs k = nth : josephus excess k
  where excess = take (length xs - 1) (drop k (cycle xs))
        nth = (cycle xs) !! (k - 1)

-}
