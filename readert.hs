(>=>) :: Monad m => (a -> m b) -> (b -> m c) -> (a -> m c)
f >=> g = \x -> f x >>= g

(<=<) :: Monad m => (b -> m c) -> (a -> m b) -> (a -> m c)
(<=<) = flip (>=>)

newtype ReaderT r m a = ReaderT { runReaderT :: r -> m a }

instance (Functor m) => Functor (ReaderT r m) where
  fmap f m = ReaderT $ \r -> fmap f (runReaderT m r)

instance (Applicative m) => Applicative (ReaderT r m) where
  pure r = ReaderT $ \_ -> pure r

  f <*> v = ReaderT $ \r -> runReaderT f r <*> runReaderT v r

instance (Monad m) => Monad (ReaderT r m) where
  return = pure

  m >>= k = ReaderT $ \r -> do
    a <- runReaderT m r
    runReaderT (k a) r

liftReaderT :: m a -> ReaderT r m a
liftReaderT m = ReaderT $ \r -> m
-- liftReaderT m = ReaderT (const m)

ask :: Monad m => ReaderT r m r
ask = ReaderT return

local :: (r -> r) -> ReaderT r m a -> ReaderT r m a
local f m = ReaderT $ \r -> runReaderT m (f r)

printEnv :: ReaderT String IO ()
printEnv = do
  env <- ask
  liftReaderT $ putStrLn ("Here's " ++ env)
  local (const "local env") $ do
    env' <- ask
    liftReaderT $ putStrLn ("Here's " ++ env')

main :: IO ()
main = 
  runReaderT printEnv "env1"
