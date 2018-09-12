-- https://www.codewars.com/kata/55c45be3b2079eccff00010f/haskell

module Codewars.Kata.YourOrderPlease where

getOrder = head . filter isDigit

yourOrderPlease :: String -> String
yourOrderPlease = words . sortBy (comparing getOrder) . words
