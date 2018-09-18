import Prelude
import Control.Monad.State
import Data.List

data Card =
  C_2 | C_3 | C_4 | C_5 | C_6 | C_7 |
  C_8 | C_9 | C_10 | C_J | C_Q | C_K | C_A
  deriving (Eq, Ord, Enum, Show)

type CardStack = [Card]

countCards :: State CardStack Int
countCards = state $ \s -> (length s, s)
-- or countCards = pure . length

sortCards :: State CardStack ()
sortCards = state $ \s -> ((), sort s)

popCard :: State CardStack Card
popCard = state $ \s -> (head s, tail s)

pushCard :: Card -> State CardStack ()
pushCard c = state $ \s -> ((),  c : s)

op :: State CardStack Card
op = do
  put [C_8, C_J, C_2, C_10, C_3, C_4, C_A]
  modify sort
  
  replicateM_ 3 $ do
    cs <- get
    when (length cs < 10) $ do
      pushCard C_2

  popCard

main :: IO ()
main = do
  print $ runState op []
