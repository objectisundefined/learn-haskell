{-
Once upon a time, on a way through the old wild west,…
… a man was given directions to go from one point to another. The directions were "NORTH", "SOUTH", "WEST", "EAST". Clearly "NORTH" and "SOUTH" are opposite, "WEST" and "EAST" too. Going to one direction and coming back the opposite direction is a needless effort. Since this is the wild west, with dreadfull weather and not much water, it's important to save yourself some energy, otherwise you might die of thirst!

How I crossed the desert the smart way.
The directions given to the man are, for example, the following:

["NORTH", "SOUTH", "SOUTH", "EAST", "WEST", "NORTH", "WEST"].
or

{ "NORTH", "SOUTH", "SOUTH", "EAST", "WEST", "NORTH", "WEST" };
or (haskell)

[North, South, South, East, West, North, West]
You can immediatly see that going "NORTH" and then "SOUTH" is not reasonable, better stay to the same place! So the task is to give to the man a simplified version of the plan. A better plan in this case is simply:

["WEST"]
or

{ "WEST" }
or (haskell)

[West]
or (rust)

[WEST];
Other examples:
In ["NORTH", "SOUTH", "EAST", "WEST"], the direction "NORTH" + "SOUTH" is going north and coming back right away. What a waste of time! Better to do nothing.

The path becomes ["EAST", "WEST"], now "EAST" and "WEST" annihilate each other, therefore, the final result is [] (nil in Clojure).

In ["NORTH", "EAST", "WEST", "SOUTH", "WEST", "WEST"], "NORTH" and "SOUTH" are not directly opposite but they become directly opposite after the reduction of "EAST" and "WEST" so the whole path is reducible to ["WEST", "WEST"].

Task
Write a function dirReduc which will take an array of strings and returns an array of strings with the needless directions removed (W<->E or S<->N side by side).

The Haskell version takes a list of directions with data Direction = North | East | West | South. The Clojure version returns nil when the path is reduced to nothing. The Rust version takes a slice of enum Direction {NORTH, SOUTH, EAST, WEST}.

Examples
dirReduce [North, South, South, East, West, North, West] `shouldBe` [West]
dirReduce [North, South, South, East, West, North] `shouldBe` []

-}

{-
module Codewars.Kata.Reduction.Test where
import Codewars.Kata.Reduction (dirReduce)
import Codewars.Kata.Reduction.Direction
import Control.Monad
import Test.Hspec
import Test.QuickCheck

main :: IO ()
main = hspec $ do
  describe "dirReduce - simple tests" $ do
    it "should work for some small examples" $ do
      dirReduce [     ]             `shouldBe` [     ]
      dirReduce [North]             `shouldBe` [North]
      dirReduce [North, West]       `shouldBe` [North,West]      
      dirReduce [North, West, East] `shouldBe` [North]
      dirReduce [North, West, South, East] `shouldBe` [North, West, South, East]
      dirReduce [North, South, South, East, West, North, West] `shouldBe` [West]
      dirReduce [North, South, South, East, West, North]       `shouldBe` []
      
    it "should return a single direction unchanged" $ do
      property $ forAll (elements [North, West, East, South]) $ \x ->
        dirReduce [x] `shouldBe` [x]
        
    it "should return a random list of north-west unchanged" $ do
      property $ forAll (listOf $ elements [North, West]) $ \xs ->
        dirReduce xs `shouldBe` xs
        
  describe "running there and back again" $ do
    forM_ [(North, South), (South, North), (West, East), (East, West)] $ \(a,b) ->
      it ("should return the right amount of " ++ show a) $ 
        property $ \t y -> t > 0 && y > 0 ==>
          let x = t + y
          in dirReduce (replicate x a ++ replicate y b) `shouldBe` replicate (x - y) a

-}

module Codewars.Kata.Reduction where
import Codewars.Kata.Reduction.Direction

-- data Direction = North | East | West | South deriving (Eq)

dirReduce :: [Direction] -> [Direction]
dirReduce l = case l of
  [] -> []
  [x] -> l
  (a : b : xs) -> if ((a == North && b == South) || (a == East && b == West) || (b == North && a == South) || (b == East && a == West))
                  then dirReduce xs
                  else merge a (dirReduce (b : xs))

merge :: Direction -> [Direction] -> [Direction]
merge a l = case l of
  [] -> [a]
  (x : xs) -> if ((a == North && x == South) || (a == East && x == West) || (x == North && a == South) || (x == East && a == West))
    then xs
    else a : l

{-
dirReduce :: [Direction] -> [Direction]
dirReduce = foldr collapse []

collapse North (South:xs) = xs
collapse South (North:xs) = xs
collapse East (West:xs) = xs
collapse West (East:xs) = xs
collapse x xs = x:xs

-}

{-

dirReduce :: [Direction] -> [Direction]
dirReduce xs = let r = go xs in if r == xs then r else dirReduce r
  where go (North : South : xs) = go xs
        go (South : North : xs) = go xs
        go (West  : East  : xs) = go xs
        go (East  : West  : xs) = go xs
        go (x             : xs) = x : go xs
        go [                  ] = []

-}

{-
oppositeDirection :: Direction -> Direction -> Bool
oppositeDirection North South = True
oppositeDirection South North = True
oppositeDirection East  West  = True
oppositeDirection West  East  = True
oppositeDirection _     _     = False

removePairs :: [Direction] -> [Direction]
removePairs (x:y:xs) | oppositeDirection x y = removePairs xs
                     | otherwise             = x:removePairs (y:xs)
removePairs xs = xs

dirReduce :: [Direction] -> [Direction]
dirReduce s | s == s'   = s
            | otherwise = dirReduce s'
    where s' = removePairs s

-}
