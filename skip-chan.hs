import Control.Concurrent
import Control.Monad
import System.IO

data SkipChan a = SkipChan (MVar (a, [MVar ()])) (MVar ())

newSkipChan :: a -> IO (SkipChan a)
newSkipChan a = do
    sem <- newEmptyMVar
    main <- newMVar (a, [sem])
    return (SkipChan main sem)

putSkipChan :: SkipChan a -> a -> IO ()
putSkipChan (SkipChan main _) v = do
    (_, sems) <- takeMVar main
    putMVar main (v, [])
    mapM_ (\sem -> putMVar sem ()) sems

getSkipChan :: SkipChan a -> IO a
getSkipChan (SkipChan main sem) = do
    takeMVar sem
    (v, sems) <- takeMVar main
    putMVar main (v, sem:sems)
    return v

dupSkipChan :: SkipChan a -> IO (SkipChan a)
dupSkipChan (SkipChan main _) = do
    sem <- newEmptyMVar
    (v, sems) <- takeMVar main
    putMVar main (v, sem:sems)
    return (SkipChan main sem)

main :: IO ()
main = do
  hSetBuffering stdout LineBuffering
  chan <- newSkipChan undefined :: IO (SkipChan Int)
  forkIO (forever (dupSkipChan chan >>= getSkipChan >>= (\v -> putStrLn ("[write]  Hello " ++ show v))))
  forkIO (forever (dupSkipChan chan >>= getSkipChan >>= (\v -> putStrLn ("[write]  world " ++ show v))))
  forkIO (forever (dupSkipChan chan >>= \ ch -> putStrLn "[action] waiting..." >> getSkipChan ch >> putStrLn "[action] notify"))
  forkIO (forever (getSkipChan chan >>= \ v -> threadDelay 1000000 >> putSkipChan chan (v + 1)))
  forkIO (forever (getSkipChan chan >>= \ v -> threadDelay 1000000 >> putSkipChan chan (v - 1)))
  forkIO (putSkipChan chan 0)
  threadDelay 10000000

-- https://hackage.haskell.org/package/base-4.15.0.0/docs/src/Control-Concurrent-MVar.html
