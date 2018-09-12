{-
Create a function named divisors/Divisors that takes an integer and returns an array with all of the integer's divisors(except for 1 and the number itself). If the number is prime return the string '(integer) is prime' (null in C#) (use Either String a in Haskell and Result<Vec<u32>, String> in Rust).

Example:
divisors 12   -- should return Right [2,3,4,6]
divisors 13   -- should return Left "13 is prime"
divisors 25   -- should return Right [5]
You can assume that you will only get positive integers as inputs.

-}

module Divisors where

divisors :: (Show a, Integral a) => a -> Either String [a]
divisors n = if length l == 0 then Left (show n ++ " is prime") else Right l
where l = factors n

factors :: Integral a => a -> [a]
factors n = acc n (n - 1) []
where acc n i res = if i <= 1 then res
                              else acc n (i - 1) (if n `mod` i == 0 then i : res else res)

{-
module Divisors where

divisors :: Integer -> Either String [Integer]
divisors a = if null l
           then Left (show a ++ " is prime")
           else Right l
        where l = [x | x <- [2..a`div`2], a`mod`x == 0]

-}

{-
divisors :: (Show a, Integral a) => a -> Either String [a]
divisors a
  | null divs = Left $ (show a) ++ " is prime"
  | otherwise = Right divs
  where divs = filter ((==0) . mod a) [2..(a-1)]
  
-}

{-
null [] = True
null (x : xs) = Flase
-}
