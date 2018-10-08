{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

data Nat = Zero | Succ Nat

data SNat a where
  SZero :: SNat 'Zero
  SSucc :: SNat a -> SNat ('Succ a)

data Vec :: * -> Nat -> * where
  VNil :: Vec a 'Zero
  VCons :: a -> Vec a n -> Vec a ('Succ n)

type family (a :: Nat) :< (b :: Nat) where
  m         :< 'Zero     = 'False
  'Zero     :< 'Succ n   = 'True
  ('Succ m) :< ('Succ n) = m :< n

vecIndex :: ((a :< b) ~ 'True) => SNat a -> Vec x b -> x
vecIndex SZero (VCons x _) = x
vecIndex (SSucc n) (VCons _ xs) = vecIndex n xs

-- example
vecOne :: Vec Nat (Succ Zero)
vecOne = VCons Zero VNil
vecTwo :: Vec Nat (Succ (Succ Zero))
vecTwo = VCons Zero $ VCons (Succ Zero) VNil

_ = vecIndex (SSucc SZero) vecTwo

{-
_ = vecIndex (SSucc $ SSucc SZero) vecTwo

Couldn't match type ‘'False’ with ‘'True’
arising from a use of ‘vecIndex’

-}

{-
https://zhuanlan.zhihu.com/p/31690842

-}
