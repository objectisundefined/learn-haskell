import Prelude hiding (null, map, filter)
import Data.HashSet
import Data.Char

hashSet = fromList ['a', 'b', 'c']

main :: IO ()
main = do
  print $ hashSet -- fromList "abc"

  print $ null hashSet -- False
  print $ size hashSet -- 3

  print $ member 'a' hashSet -- True
  print $ member 'e' hashSet -- False

  print $ insert 'd' hashSet -- fromList "abcd"
  print $ delete 'b' hashSet -- fromList "ac"

  print $ map (toUpper) hashSet -- fromList "ABC"
  print $ filter (> 'a') hashSet -- fromList "bc"
