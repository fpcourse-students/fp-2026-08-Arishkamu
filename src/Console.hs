-- | Учебная консоль. Конструктор MyIO скрыт; World доступен для проверки.
module Console
  ( MyIO
  , World (..)
  , world
  , runMyIO
  , readInt
  , readIntMaybe
  , writeInt
  ) where

newtype MyIO a = MyIO
  { runMyIO :: World -> (World, a)
  }

data World = World
  { worldInput :: [Int]
  , worldOutput :: [Int]
  }
  deriving (Eq, Show)

world :: [Int] -> World
world xs = World xs []

instance Functor MyIO where
  fmap f (MyIO action) = MyIO $ \w ->
    let (w', x) = action w in
    (w', f x)

instance Applicative MyIO where
  pure x = MyIO (\w -> (w, x))

  MyIO fs <*> MyIO xs = MyIO $ \w ->
    let (w', f) = fs w in
    let (w'', x) = xs w' in
    (w'', f x)

instance Monad MyIO where
  MyIO action >>= k = MyIO $ \w ->
    let (w', x) = action w in
    runMyIO (k x) w'

readInt :: MyIO Int
readInt = MyIO $ \w -> case worldInput w of
  [] -> error "readInt: input is empty"
  x : xs -> (w { worldInput = xs }, x)

readIntMaybe :: MyIO (Maybe Int)
readIntMaybe = MyIO $ \w -> case worldInput w of
  [] -> (w, Nothing)
  x : xs -> (w { worldInput = xs }, Just x)

writeInt :: Int -> MyIO ()
writeInt x = MyIO $ \w ->
  (w { worldOutput = worldOutput w ++ [x] }, ())
