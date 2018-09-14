{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE KindSignatures #-}

import Data.Foldable

-- implement foldMap
cfoldMap :: forall m a (t :: * -> *) . (Foldable t, Monoid m) => (a -> m) -> t a -> m
cfoldMap f = foldr (mappend . f) mempty

data BinaryTree a
  = Leaf a
  | Branch a (BinaryTree a) (BinaryTree a)
  | Empty
  deriving (Show)

instance Functor BinaryTree where
  fmap f tree = case tree of
    Empty -> Empty
    Leaf a -> Leaf (f a)
    Branch a l r -> Branch (f a) (fmap f l) (fmap f r)

instance Foldable BinaryTree where
  -- foldMap :: Monoid m => (a -> m) -> BinaryTree a -> m
  foldMap f tree = case tree of
    Empty -> mempty
    Leaf a -> f a
    -- Branch a l r -> (f a) `mappend` (foldMap f l) `mappend` (foldMap f r)
    -- Branch a l r -> (foldMap f l) `mappend` (f a) `mappend` (foldMap f r)
    Branch a l r -> (foldMap f l) `mappend` (foldMap f r) `mappend` (f a)

tree = Branch 1 (Branch 2 (Leaf 4) Empty) (Branch 3 Empty (Leaf 9)) 

main :: IO ()
main = do
  putStrLn . show $ tree
  putStrLn . show $ (foldMap (\a -> [a]) tree)
  putStrLn . show $ (fmap (\a -> a * 2) tree)
  putStrLn . show $ (toList tree)
  putStrLn . show $ (length tree)
  putStrLn . show $ (maximum tree)

{-
ghci enable extension

https://stackoverflow.com/questions/12584884/how-do-i-enable-language-extensions-from-within-ghci
:set -X[extension name]

about iterate
https://www.cnblogs.com/weidagang2046/p/nature-of-type.html

-}
