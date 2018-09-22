{-# LANGUAGE Rank2Types #-}

module Lens where

import Data.Functor.Identity  (Identity(..))
import Control.Applicative    (Const(..))
import Data.Function          ((&))

type Lens b a = forall f . Functor f => (a -> f a) -> b -> f b

-- over :: ((a -> Identity a) -> b -> Identity b) -> (a -> a) -> b -> b
over :: Lens b a -> (a -> a) -> b -> b
over lens f = runIdentity . lens (Identity . f)

-- set :: ((a -> Identity a) -> b -> Identity b) -> a -> b -> b
set :: Lens b a -> a -> b -> b
set lens a = over lens (const a)

view :: ((a -> Const a a) -> b -> Const a b) -> b -> a
view lens = getConst . (lens Const)

-- infix over
(%~) :: Lens b a -> (a -> a) -> b -> b
(lens %~ f) x = over lens f x
infixr 4 %~

-- infix set
(.~) :: Lens b a -> a -> b -> b
(.~) = set
infixr 4 .~

-- infix view
(^.) :: b -> Lens b a -> a
(^.) = flip view
infixl 8 ^.

data Position = Position {
    positionX :: Double
  , positionY :: Double
} deriving Show

xLens :: Lens Position Double
xLens f p = fmap (\x' -> p { positionX = x' }) $ f (positionX p)

yLens :: Lens Position Double
yLens f p = fmap (\y' -> p { positionY = y' }) $ f (positionY p)

data Hero = Hero {
    heroLevel :: Int
  , weapon :: Weapon
} deriving Show

data Weapon = Weapon {
    basicAttack :: Int
  , weaponLevel :: Int
  , magicGem :: Gem
} deriving Show

data Gem = Gem {
    gemLevel :: Int
  , gemName :: String
} deriving Show

gem :: Gem
gem = Gem {
    gemLevel = 3
  , gemName = "gemName"
}

gun :: Weapon
gun = Weapon {
    basicAttack = 80
  , weaponLevel = 2
  , magicGem = gem
}

hero :: Hero
hero = Hero {
    heroLevel = 1
  , weapon = gun
}

heroLevelLens f s = fmap (\a' -> s { heroLevel = a' }) $ f (heroLevel s)
weaponLens f s = fmap (\a' -> s { weapon = a' }) $ f (weapon s)
basicAttackLens f s = fmap (\a' -> s { basicAttack = a' }) $ f (basicAttack s)
weaponLevelLens f s = fmap (\a' -> s { weaponLevel = a' }) $ f (weaponLevel s)
magicGemLens f s = fmap (\a' -> s { magicGem = a' }) $ f (magicGem s)
gemLevelLens f s = fmap (\a' -> s { gemLevel = a' }) $ f (gemLevel s)
gemNameLens f s = fmap (\a' -> s { gemName = a' }) $ f (gemName s)

-- gemLevel . magicGem . weapon $ hero
-- hero & weapon & magicGem & gemLevel

(|>) f g = g . f
infixr 4 |>

-- hero & (weapon |> magicGem |> gemLevel)

main = do
  let p = Position 123 456
  putStrLn $ "orign value: " ++ show p
  putStrLn $ "over xLens negate p:     " ++ show (over xLens negate p)
  putStrLn $ "set xLens 0 p:           " ++ show (set xLens 0 p)
  putStrLn $ "view yLens p:            " ++ show (view yLens p)
  putStrLn $ "p & xLens %~ negate:     " ++ show (p & xLens %~ negate)
  putStrLn $ "p & xLens .~ 0:          " ++ show (p & xLens .~ 0)
  putStrLn $ "p ^. yLens:              " ++ show (p ^. yLens)
  putStrLn $ "nested over lens:        " ++ show (hero & (weaponLens . magicGemLens . gemLevelLens) %~ (+1))
  putStrLn $ "nested set lens:         " ++ show (hero & (weaponLens . magicGemLens . gemLevelLens) .~ 25)
  putStrLn $ "nested view lens:        " ++ show (hero ^. (weaponLens . magicGemLens . gemLevelLens))
