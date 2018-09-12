module Codewars.BuildTower where

buildTower :: Int -> [String]
buildTower n = case n of
  1 -> ["*"]
  _ -> (map padding $ buildTower (n - 1)) ++ [take (2 * n - 1) $ repeat '*']

  where
    padding l = " " ++ l ++ " "

{-
buildTower :: Int -> [String]
buildTower n = [sp x ++ st x ++ sp x | x <- [1..n]]
  where
    sp x = replicate (n - 1) ' '
    st x = replicate (2 * n - 1) '*'
-}

{-
  n = 2
  [
    '  *  ', 
    ' *** ', 
    '*****'
  ]
-}

{-
  n = 3
  [
    '     *     ', 
    '    ***    ', 
    '   *****   ', 
    '  *******  ', 
    ' ********* ', 
    '***********'
  ]
-}

-- Data.List (replicate)
-- replicate :: Int -> a -> [a]
-- replicate n a = take n $ repeat a
