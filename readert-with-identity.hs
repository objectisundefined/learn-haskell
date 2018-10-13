import Data.Coerce

newtype Identity a = Identity { runIdentity :: a } deriving Show

instance Functor Identity where
  fmap = coerce

instance Applicative Identity where
  pure = Identity
  (<*>) = coerce

instance Monad Identity where
  return = pure
  m >>= k = k $ runIdentity m

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
-- liftReaderT m = ReaderT $ \ r -> m
liftReaderT m = ReaderT (const m)

ask :: Monad m => ReaderT r m r
ask = ReaderT return

local :: (r -> r) -> ReaderT r m a -> ReaderT r m a
local f m = ReaderT $ \r -> runReaderT m (f r)

headT :: ReaderT String Identity String
headT = do
  name <- ask
  return $ "Welcome! " ++ name ++ ".\n"

bodyT :: ReaderT String Identity String
bodyT = do
  name <- ask
  return $
    "Welcome to my home, "
    ++ name
    ++ ". This's the best home you can ever find on this planet!.\n"

footT :: ReaderT String Identity String
footT = do
  name <- ask
  return $ "Now help yourself, " ++ name ++ ".\n"

data Greet = Greet {
    guestName :: String
  , greettHead :: String
  , greetBody :: String
  , greetFoot :: String
} deriving Show

-- isomorphic with Reader String Greet
renderGreeting :: ReaderT String Identity Greet
renderGreeting = do
  n <- ask
  h <- headT
  (b, f) <- local ("Mr. and Mrs. " ++) $ do
    b' <- bodyT
    f' <- footT
    return (b', f')
  return $ Greet n h b f

printGreeting :: ReaderT String Identity (IO ())
printGreeting = (liftReaderT (Identity (putStrLn . show))) <*> renderGreeting

main :: IO ()
main = runIdentity $ runReaderT printGreeting "Mike"
