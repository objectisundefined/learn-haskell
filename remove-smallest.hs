-- Remove the frist minimum

module Codewars.Kata.RemoveSmallest where

{-
removeSmallest :: [Int] -> [Int]
removeSmallest xs = case xs of
  [] -> []
  l@(x : xs) -> if x == min then xs else x : removeSmallest xs
    where min = minimum l
-}  

removeSmallest :: [Int] -> [Int]
removeSmallest xs = removeSmallest' xs (minimum xs) []

removeSmallest' :: [Int] -> Int -> [Int] -> [Int]
removeSmallest' l v acc = case l of
  [] -> acc
  (x : xs) -> if x `eq` v then (reverse acc) ++ xs else removeSmallest' xs v (x : acc)

-- Data.List (delete)
-- removeSmallest :: Ord a => [a] -> [a]
-- removeSmallest xs = delete (minimum xs) xs

-- Data.List (delete)
-- removeSmallest :: Ord a => [a] -> [a]
-- removeSmallest = delete =<< minimum
-- or removeSmallest = minimum >>= delete

-- https://hackage.haskell.org/package/base-4.8.2.0/docs/src/GHC.Base.html#line-621

{-
instance Functor ((->) r) where
  fmap = (.)

instance Applicative ((->) a) where
  pure = const
  (<*>) f g x = f x (g x)

instance Monad ((->) r) where
  return = const
  f >>= k = \ r -> k (f r) r

-}

-- https://www.zhihu.com/question/53094776
-- isPalindrome :: Eq a => [a] -> Bool
-- isPalindrome = (==) <*> reverse

