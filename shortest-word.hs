module FindShortest where

import Data.List

find_shortest :: String -> Integer
find_shortest = toInteger . minimum . map length . words
