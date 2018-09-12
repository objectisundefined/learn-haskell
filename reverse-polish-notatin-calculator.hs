{-
Your job is to create a calculator which evaluates expressions in Reverse Polish notation.

For example expression 5 1 2 + 4 * + 3 - (which is equivalent to 5 + ((1 + 2) * 4) - 3 in normal notation) should evaluate to 14.

Note that for simplicity you may assume that there are always spaces between numbers and operations, e.g. 1 3 + expression is valid, but 1 3+ isn't.

Empty expression should evaluate to 0.

Valid operations are +, -, *, /.

You may assume that there won't be exceptional situations (like stack underflow or division by zero).

-- tests

module RPN.Test where
import qualified RPN
import Test.Hspec
import Test.QuickCheck

describe "The rpn evaluator" $ do
  it "should work with the empty string" $
    RPN.calc "" `shouldBe` 0
  it "should parse numbers" $
    RPN.calc "1 2 3" `shouldBe` 3
  it "should parse float numbers" $
    RPN.calc "1 2 3.5" `shouldBe` 3.5
  it "should support addition" $
    RPN.calc "1 3 +" `shouldBe` 4
  it "should support multiplication" $
    RPN.calc "1 3 *" `shouldBe` 3
  it "should support substraction" $
    RPN.calc "1 3 -" `shouldBe` (-2)
  it "should support division" $
    RPN.calc "4 2 /" `shouldBe` 2
-}

module RPN where

import Prelude
import Data.List (findIndex)
import Data.Char (isDigit)

calc :: String -> Double
calc s = eval $ words s

op :: String -> String -> String -> Double
op a b s =
  let
    a' = read a :: Double
    b' = read b :: Double
  in
    case s of
      "+" -> a' + b'
      "*" -> a' * b'
      "/" -> a' / b'
      _ -> a' - b'


eval :: [String] -> Double
eval expr =
  let
    -- case ["1", "-2.0", "+"], not .isDight . head is incorrect
    m = findIndex (not . any isDigit) expr
  in
    case m of
      Just i -> eval $ (take (i - 2) expr) ++ [show $ op (expr !! (i - 2)) (expr !! (i - 1)) (expr !! i)] ++ (drop (i + 1) expr)
      Nothing -> if (length expr > 0) then read (last expr) :: Double else 0

{-
main :: IO ()
main = do
    print (calc "")
    print (calc "1 2 3")
    print (calc "1 2 3.5")
    print (calc "1 3 +")
    print (calc "1 3 *")
    print (calc "1 3 -")
    print (calc "4 2 /")
-}

{-
data Polish t = Number t | Operator (t -> t -> t)

polish :: String -> Polish Double
polish "+" = Operator (+)
polish "-" = Operator (-)
polish "*" = Operator (*)
polish "/" = Operator (/)
polish n = Number (read n)

eval acc (Number n) = n : acc
eval (a:b:acc) (Operator op) = (op b a) : acc

calc :: String -> Double
calc = head . foldl eval [0] . map polish . words

-}

{-
calc :: String -> Double
calc = head . foldl' go [0] . words where
  go (x:y:ss) "+" = (y + x) : ss
  go (x:y:ss) "-" = (y - x) : ss
  go (x:y:ss) "*" = (y * x) : ss
  go (x:y:ss) "/" = (y / x) : ss
  go ss n         = read n : ss

-}
