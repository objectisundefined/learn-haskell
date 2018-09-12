{-
Pete likes to bake some cakes. He has some recipes and ingredients. Unfortunately he is not good in maths. Can you help him to find out, how many cakes he could bake considering his recipes?

Write a function cakes(), which takes the recipe (object) and the available ingredients (also an object) and returns the maximum number of cakes Pete can bake (integer). For simplicity there are no units for the amounts (e.g. 1 lb of flour or 200 g of sugar are simply 1 or 200). Ingredients that are not present in the objects, can be considered as 0.

Examples:

cakes [("flour",500), ("sugar",200), ("eggs",1)] [("flour",1200), ("sugar",1200), ("eggs",5), ("milk",200)]  `shouldBe` 2
cakes [("apples",3), ("flour",300), ("sugar",150), ("milk",100), ("oil",100)] [("sugar",500), ("flour",2000), ("milk",2000)] `shouldBe` 0

-}

module Baker where

import Prelude
import Data.List (find)

type Ingredient = String
type Amount     = Int
type Recipe     = [(Ingredient, Amount)]
type Storage    = [(Ingredient, Amount)]

cakes :: Recipe -> Storage -> Int
cakes recipe storage = minimum $ map (\(i, a) -> maybe 0 (`div` a) $ lookup i storage) recipe

{-
import Prelude hiding (lookup)

cakes :: Recipe -> Storage -> Int
cakes recipe storage = minimum $ map (\(i, a) -> (lookup i storage) `div` a) $ recipe

lookup :: Ingredient -> Storage -> Amount
lookup ingredient storage =
  let
    Just (i, a) = find (\(i, a) -> i == ingredient) (storage ++ [(ingredient, 0)])
  in
    a
-}

{-
import Prelude (lookup)

lookup :: Eq a => a -> [a] -> Maybe b
-}

{-
Prelude maybe

maybe :: b -> (a -> b) -> Maybe a -> b
-}

-- use language extention
-- {-# LANGUAGE ViewPatterns #-}
{-
module Baker where

  import qualified Data.Map.Lazy as Map
  
  type Ingredient = String
  type Amount     = Int
  type Recipe     = [(Ingredient, Amount)]
  type Storage    = [(Ingredient, Amount)]
  
  cakes :: Recipe -> Storage -> Int
  cakes recipe (Map.fromList -> storage) = 
    foldl (\acc (i,a) -> min acc (Map.findWithDefault 0 i storage `div` a) ) maxBound recipe
-}
