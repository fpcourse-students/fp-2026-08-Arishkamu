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
identityCounterexample = todo "2.1 Identity"

newtype PointN a = PointN { getPointN :: [a] }
  deriving (Eq, Show, Functor)

todoImpl ''Applicative ''PointN


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

todoImpl ''Functor ''PointI

todoImpl ''Applicative ''PointI


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

todoImpl ''Functor ''Compose

todoImpl ''Applicative ''Compose


-- 2.4. Сравнение бинарных функций.
--
-- Сравните бинарные функции на всех парах входов конечных областей.
-- Используйте обычные списки для перебора всех пар
-- и ZipList для сравнения результатов.
-- Области должны быть малыми: например, Bool или Ordering, не Int.

type Walkable a = (Enum a, Bounded a)

sameBinary :: (Walkable a, Walkable b, Eq c) => (a -> b -> c) -> (a -> b -> c) -> Bool
sameBinary = todo "2.4"


-- 2.5. Разное: лучший игрок.
--
-- Найдите игрока с наибольшим количеством очков.
-- Для пустого списка верните Nothing. Если максимум набрали несколько
-- игроков, верните первого из них в исходном списке.
-- Способ решения выберите сами.

bestPlayer :: [Player] -> Maybe Player
bestPlayer = todo "2.5"
