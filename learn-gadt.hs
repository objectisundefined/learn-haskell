{-# LANGUAGE GADTs #-}

data Expr a where
  ILit :: Int -> Expr Int
  BLit :: Bool -> Expr Bool
  Add :: Expr Int -> Expr Int -> Expr Int
  Eq :: Eq a => Expr a -> Expr a -> Expr Bool

addInt = Add (ILit 1) (ILit 2)
eqBool = Eq (BLit True) (BLit False)

data Z
data S n

data Stack t where
  SBegin :: Stack Z
  SPush :: Int -> Stack t -> Stack (S t)
  SAdd :: Stack (S (S t)) -> Stack (S t)

begin = SBegin
stack = SAdd (SPush 2 (SPush 1 begin))

{-
sBug = SAdd (SPush 1 begin)

Couldn't match type ‘Z’ with ‘S t’
Expected type: Stack (S (S t))
Actual type: Stack (S Z)
-}

{-
https://colliot.me/zh/2017/11/what-is-gadt-in-haskell/

-}
