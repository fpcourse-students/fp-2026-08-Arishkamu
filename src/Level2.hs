-- | Задачи к очной сессии.
module Level2 where

import Control.Applicative (ZipList (..))
import Examples qualified as E
import Level1 (Player (..))
import MetaUtils (todo, todoImpl)


-- 2.1. Точка в многомерном пространстве.
--
-- Самостоятельно напишите инстанс Applicative для PointN.
-- (<*>) соединяет соответствующие координаты, обрезая результат
-- до меньшей длины.
-- Выберите pure так, чтобы для любой точки p выполнялось:
--
--   pure id <*> p == p
--
-- Сначала найдите конечный вход, на котором E.badIdentity из Examples.hs
-- нарушает Identity: результат отличается от исходного списка.
-- Запишите вход литералом в identityCounterexample.
-- Законы Applicative и их имена — в лекции 8, слайд 23.

identityCounterexample :: [Int]
identityCounterexample = [1, 2, 3]

newtype PointN a = PointN { getPointN :: [a] }
  deriving (Eq, Show, Functor)

instance Applicative PointN where
  pure x = PointN { getPointN = repeat x }
  PointN fs <*> PointN xs = PointN $ map (uncurry ($)) (zip fs xs)


-- 2.2. Точка с произвольными индексами.
--
-- Координаты индексируются произвольным типом i.
-- Самостоятельно напишите Functor и Applicative для PointI.
-- Операции действуют в каждой координате с одним и тем же индексом.

newtype PointI i a = PointI { at :: i -> a }

pointIExample :: PointI String Int
pointIExample = PointI $ \case
  "x" -> 3
  "y" -> 4
  unknown -> error $ "Unexpected component index: " <> unknown

instance Functor (PointI i) where
  fmap f (PointI {at = ati}) = PointI {at = f . ati}

instance Applicative (PointI i) where
  pure x = PointI (\_ -> x)

-- PointI { at :: i -> a -> b} <*> PointI { at :: i -> a } = PointI { at :: i -> b}
  PointI atf <*> PointI atx = PointI $ \ind -> atf ind (atx ind)


-- 2.3. Композиция аппликативных функторов.
--
-- Напишите полные инстансы Functor и Applicative для Compose.
-- Функция действует во внутреннем g, эффекты объединяются на обоих уровнях.
-- Укажите необходимые ограничения самостоятельно;
-- deriving здесь не используйте.
--
-- Например (числа имеют тип Int):
--   GHCi> getCompose $ fmap (+1) (Compose [Just 1, Nothing, Just 3])
--   [Just 2, Nothing, Just 4]
--
--   GHCi> getCompose $ liftA2 (+) (Compose [Just 1, Nothing]) (Compose [Just 10, Just 20])
--   [Just 11, Just 21, Nothing, Nothing]

newtype Compose f g a = Compose { getCompose :: f (g a) }
  deriving (Eq, Show)

instance (Functor f, Functor g) => Functor (Compose f g) where
  fmap h (Compose gc) = Compose $ (fmap h) <$> gc

instance (Applicative f, Applicative g) => Applicative (Compose f g) where
  pure x = Compose $ pure (pure x)

-- Compose { :: f (g (a -> b)) } <*> Compose { :: f (g a) }  = Compose { :: f (g b) }
  Compose gch <*> Compose gcx = Compose $ liftA2 (<*>) gch gcx


-- 2.4. Сравнение бинарных функций.
--
-- Сравните бинарные функции на всех парах входов конечных областей.
-- Используйте обычные списки для перебора всех пар
-- и ZipList для сравнения результатов.
-- Области должны быть малыми: например, Bool или Ordering, не Int.

type Walkable a = (Enum a, Bounded a)

sameBinary :: (Walkable a, Walkable b, Eq c) => (a -> b -> c) -> (a -> b -> c) -> Bool
-- version1
-- sameBinary f g = and [(f a b) == (g a b) | a <- [minBound..maxBound], b <- [minBound..maxBound]]
-- version2
sameBinary f g = and $ liftA2 (==) (allEntries f) (allEntries g)
  where
    allEntries func = ZipList $ liftA2 func [minBound..maxBound] [minBound..maxBound]


-- 2.5. Разное: лучший игрок.
--
-- Найдите игрока с наибольшим количеством очков.
-- Для пустого списка верните Nothing. Если максимум набрали несколько
-- игроков, верните первого из них в исходном списке.
-- Способ решения выберите сами.

{-
data Player = Player
  { playerName :: String
  , playerScore :: Integer
  }
-}

bestPlayer :: [Player] -> Maybe Player
bestPlayer = foldr (\x y -> Just $ cmpPlmb x y) Nothing
  where
    cmpPlmb pl1 Nothing = pl1
    cmpPlmb pl1 (Just pl2)
      | playerScore pl1 >= playerScore pl2 = pl1
      | otherwise = pl2
