
import Data.Foldable

data Color = Red | Black deriving (Show, Eq)
data RBTree a = Nil | Node Color a (RBTree a) (RBTree a) deriving (Show)

blacken :: RBTree a -> RBTree a
blacken t = case t of
  Node Red v l r -> Node Black v l r
  _ -> t

insert :: (Ord a) => RBTree a -> a -> RBTree a
insert t a = blacken $ insert' t a
  where
    insert' t a = case t of
      Nil -> Node Red a Nil Nil
      t'@(Node c v l r) ->
        if a < v then
          balance $ Node c v (insert' l a) r
        else if a > v then
          balance $ Node c v l (insert' r a)
        else
          t'

balance :: RBTree a -> RBTree a
balance t = case t of
  Node Black z (Node Red x a (Node Red y b c)) d -> Node Red y (Node Black x a b) (Node Black z c d)
  Node Black z (Node Red y (Node Red x a b) c) d -> Node Red y (Node Black x a b) (Node Black z c d)
  Node Black x a (Node Red y b (Node Red z c d)) -> Node Red y (Node Black x a b) (Node Black z c d)
  Node Black x a (Node Red z (Node Red y b c) d) -> Node Red y (Node Black x a b) (Node Black z c d)
  _ -> t

-- select middle value as top
{-
         Bz
       Rx  d
      a  Ry
        b  c
-}

{-
        Bz
      Ry  d
    Rx  c
   a  b
-}

{-
      Bx
     a  Ry
       b  Rz
         c  d
-}

{-
      Bx
    a   Rz
      Ry  d
     b  c
-}

isBlack (Node Red _ _ _) = False
isBlack _ = True

balL color y (left, True) right = (Node color y left right, True)
balL color y (left, False) right = balL' color y left right

balR color y left (right, True) = (Node color y left right, True)
balR color y left (right, False) = balR' color y left right

balL' color1 p n (Node color2 s sl sr)
  | color2 == Red = balL Black s (balL' Red p n sl) sr
  | isBlack sl && isBlack sr = (Node Black p n (Node Red s sl sr), color1 == Red)
  | not (isBlack sr) = (Node color1 s (Node Black p n sl) (blacken sr), True)
  | otherwise = let (Node Red x sll slr) = sl in balL' color1 p n (Node Black x sll (Node Red s slr sr))

balR' color1 p (Node color2 s sl sr) n
  | color2 == Red = balR Black s sl (balR' Red p sr n)
  | isBlack sl && isBlack sr = (Node Black p (Node Red s sl sr) n, color1 == Red)
  | not (isBlack sl) = (Node color1 s (blacken sl) (Node Black p sr n), True)
  | otherwise = let (Node Red x srl srr) = sr in balR' color1 p (Node Black x (Node Red s sl srl) srr) n

delete x t = fst $ delete' x t
  where
    delete' x Nil = (Nil, True)
    delete' x root@(Node color y left right)
        | x < y = balL color y (delete' x left) right
        | x > y = balR color y left (delete' x right)
        | otherwise = deleteRoot root

    deleteRoot (Node color _ Nil Nil) = (Nil, color == Red)
    deleteRoot (Node _ _ left Nil) = (blacken left, True)
    deleteRoot (Node _ _ Nil right) = (blacken right, True)
    deleteRoot (Node color _ left right) = let m = findMin right in balR color m left (delete' m right)

    findMin (Node _ x Nil _) = x
    findMin (Node _ _ left _) = findMin left

-- test :: RBTree -> Boolean
test = f . height
  where
    f [] = True
    f (x : xs) = foldl' (\_ -> \x' -> x == x') True xs

height :: RBTree a -> [Int]
height t = case t of
  Nil -> [0]
  Node c _ l r -> (\n -> map (\ll -> n + ll) (height l) ++ map (\lr -> n + lr) (height r)) $ if c == Red then 0 else 1

main :: IO ()
main = do
  putStrLn . show $ foldl' insert  Nil [1..10]
  putStrLn . show $ delete 7 $ foldl' insert  Nil [1..10]
  putStrLn . show . height $ foldl' insert  Nil [1..10]
  putStrLn . show . height $ delete 7 $ foldl' insert  Nil [1..10]
  putStrLn . show . test $ foldl' insert  Nil [1..10]
  putStrLn . show . test $ delete 7 $ foldl' insert  Nil [1..10]


-- foldl' insert  Nil [1..10]
-- Node Black 4 (Node Black 2 (Node Black 1 Nil Nil) (Node Black 3 Nil Nil)) (Node Black 6 (Node Black 5 Nil Nil) (Node Red 8 (Node Black 7 Nil Nil) (Node Black 9 Nil (Node Red 10 Nil Nil))))

{-
                                   B4
           B2                                      B6
   B1              B3                         B5          R8
  .  .            .  .                       .  .     B7     B9
                                                     .  .  .  R10
                                                              .  .
-}

-- delete 7 $ foldl' insert  Nil [1..10]
-- Node Black 4 (Node Black 2 (Node Black 1 Nil Nil) (Node Black 3 Nil Nil)) (Node Black 6 (Node Black 5 Nil Nil) (Node Red 9 (Node Black 8 Nil Nil) (Node Black 10 Nil Nil)))

{-
                                   B4
           B2                                      B6
   B1              B3                         B5        R9
  .  .            .  .                       .  .     B8   B10
                                                     .  .  .  .

-}

-- https://zhuanlan.zhihu.com/p/77616103
