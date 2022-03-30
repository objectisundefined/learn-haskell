import Control.Monad.RWS

world :: Bool -> RWS Int [String] Int Bool
world a = rws (\ r s -> (a, s, []))

eg :: (Bool, Int, [String])
eg = runRWS (world True) 0 2

program :: RWS Int [String] Int Bool
program = do
  world True
  r <- ask
  tell ["step 1 - constructor with True"]
  put r
  tell ["step 2 - put " ++ show r]
  modify (+ 10)
  tell ["step 3 - modify (+ 10)"]
  a <- get
  put (a + 12)
  tell ["step 4 - modify (+ 12)"]
  even <$> get

main :: IO ()
main = print $ runRWS program 2 0

-- 如何理解Monad Transformer？有哪些资料可以帮助理解它？
-- https://www.zhihu.com/question/39783787
