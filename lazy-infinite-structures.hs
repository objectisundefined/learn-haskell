{-
Demonstrates Haskell's lazy evaluation and infinite data structures
- Infinite lists and streams
- Lazy computation examples
- Memory-efficient processing of infinite data
-}

module LazyInfiniteStructures where

-- Infinite list of natural numbers
naturals :: [Integer]
naturals = [0..]

-- Infinite list of fibonacci numbers using lazy evaluation
fibs :: [Integer]
fibs = 0 : 1 : zipWith (+) fibs (tail fibs)

-- Infinite list of primes using sieve of Eratosthenes
primes :: [Integer]
primes = sieve [2..]
  where
    sieve (p:xs) = p : sieve [x | x <- xs, x `mod` p /= 0]

-- Infinite binary tree
data Tree a = Node a (Tree a) (Tree a)

-- Generate infinite binary tree with level-order labeling
infiniteTree :: Tree Integer
infiniteTree = go 1
  where
    go n = Node n (go (2*n)) (go (2*n + 1))

-- Take first n levels of infinite tree
takeTree :: Int -> Tree a -> Tree a
takeTree 0 _ = error "Cannot take 0 levels"
takeTree 1 (Node x _ _) = Node x (Node undefined undefined undefined) (Node undefined undefined undefined)
takeTree n (Node x l r) = Node x (takeTree (n-1) l) (takeTree (n-1) r)

-- Infinite stream data type
data Stream a = Cons a (Stream a)

instance Functor Stream where
  fmap f (Cons x xs) = Cons (f x) (fmap f xs)

-- Stream of ones
ones :: Stream Integer
ones = Cons 1 ones

-- Stream from function
streamFrom :: (a -> a) -> a -> Stream a
streamFrom f x = Cons x (streamFrom f (f x))

-- Powers of 2
powersOf2 :: Stream Integer
powersOf2 = streamFrom (*2) 1

-- Take first n elements from stream
takeStream :: Int -> Stream a -> [a]
takeStream 0 _ = []
takeStream n (Cons x xs) = x : takeStream (n-1) xs

-- Lazy evaluation example: expensive computation only done when needed
expensiveList :: [Integer]
expensiveList = map expensiveFunction [1..1000000]
  where
    expensiveFunction n = n * n * n + n * n + n + 1

-- Only compute what we need
firstTenExpensive :: [Integer]
firstTenExpensive = take 10 expensiveList

-- Demonstrate space-efficient processing of infinite data
sumFirstN :: Int -> [Integer] -> Integer
sumFirstN n = sum . take n

-- Corecursive data structures - Pascal's triangle
pascal :: [[Integer]]
pascal = [1] : map (\row -> zipWith (+) ([0] ++ row) (row ++ [0])) pascal

-- Infinite list of factorials
factorials :: [Integer]
factorials = scanl1 (*) [1..]

-- Collatz sequence (3n+1 problem)
collatz :: Integer -> [Integer]
collatz 1 = [1]
collatz n
  | even n = n : collatz (n `div` 2)
  | odd n  = n : collatz (3 * n + 1)

-- Hamming numbers (numbers with only 2, 3, 5 as prime factors)
hamming :: [Integer]
hamming = 1 : merge (map (*2) hamming) 
                   (merge (map (*3) hamming) 
                          (map (*5) hamming))
  where
    merge (x:xs) (y:ys)
      | x < y = x : merge xs (y:ys)
      | x > y = y : merge (x:xs) ys
      | otherwise = x : merge xs ys

-- Examples of usage
examples :: IO ()
examples = do
  putStrLn "First 10 Fibonacci numbers:"
  print $ take 10 fibs
  
  putStrLn "\nFirst 10 prime numbers:"
  print $ take 10 primes
  
  putStrLn "\nFirst 10 powers of 2:"
  print $ takeStream 10 powersOf2
  
  putStrLn "\nFirst 5 rows of Pascal's triangle:"
  mapM_ print $ take 5 pascal
  
  putStrLn "\nFirst 10 Hamming numbers:"
  print $ take 10 hamming
  
  putStrLn "\nCollatz sequence for 27:"
  print $ collatz 27
  
  putStrLn "\nSum of first 100 natural numbers (computed lazily):"
  print $ sumFirstN 100 naturals

main :: IO ()
main = examples