import Control.Concurrent
import Control.Monad
import System.IO

main = do
  hSetBuffering stdout NoBuffering
  forkIO (forever (putStr "Hello"))
  forkIO (forever (putStr "World"))
  threadDelay 3000
