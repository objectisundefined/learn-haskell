-- aden Casing Strings

{-
Not Jaden-Cased: "How can mirrors be real if our eyes aren't real"
Jaden-Cased:     "How Can Mirrors Be Real If Our Eyes Aren't Real"
-}

module JadenCasing where

import Data.Char

-- toJadenCase :: String -> String
-- toJadenCase = unwords . map format . words
--   where
--     format s = [toUpper . head $ s] ++ (map toLower . tail $ s)

toJadenCase :: String -> String
toJadenCase = unwords . map capitalize . words
  where
    capitalize :: String -> String
    capitalize [] = []
    capitalize (x : xs) = toUpper x : (map toLower xs)
