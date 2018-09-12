-- Persistent Bugger
-- caculate steps to one digit

{-
persistence 39 -- returns 3, because 3*9=27, 2*7=14, 1*4=4
                -- and 4 has only one digit

 persistence 999 -- returns 4, because 9*9*9=729, 7*2*9=126,
                 -- 1*2*6=12, and finally 1*2=2

 persistence 4 -- returns 0, because 4 is already a one-digit number

-}

module Codewars.G.Persistence where

import Data.Char

persistence :: Int -> Int
persistence n | n < 10 = 0
              | otherwise = 1 + (persistence $ foldl (*) 1 $ (map digitToInt) $ show n)



-- Data.List (product)   
-- product = foldl (*) 1
