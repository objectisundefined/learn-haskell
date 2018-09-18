import Prelude
import Control.Monad

headT :: String -> String
headT name = "Welcome! " ++ name ++ ".\n"

bodyT :: String -> String
bodyT name = "Welcome to my home, "
  ++ name
  ++ ". This's the best home you can ever find on this planet!.\n"

footT :: String -> String
footT name = "Now help yourself, " ++ name ++ ".\n"

data Greet = Greet {
    guestName :: String
  , greettHead :: String
  , greetBody :: String
  , greetFoot :: String
} deriving Show

ask :: a -> a
ask = id

local :: (a -> a) -> (a -> r) -> a -> r
local f g = g . f

renderGreeting :: String -> Greet
renderGreeting = do
  n <- ask
  h <- headT
  (b, f) <- local ("Mr. and Mrs. " ++) $ do
    b' <- bodyT
    f' <- footT
    return (b', f')
  return $ Greet n h b f

main :: IO ()
main =
  print $ renderGreeting "Mike"
