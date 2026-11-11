{- HLINT ignore "Use newtype instead of data" -}

-- | Домашка 8. Аппликативные функторы: обязательные задачи.
module Level1 where

import Console (MyIO, readInt)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import MetaUtils (todo)
import Text.Read (readMaybe)


-- 1.1. Разбор записи об игроке.
--
-- Строка имеет формат «имя:очки», например "Alice:42".
-- Имя должно быть непустым, а очки — читаться как Integer через readMaybe.
-- При неверном формате верните Nothing.
--
-- Самостоятельно напишите parseName и parseScore для отдельных полей.
-- В parsePlayer соберите Player из результатов разбора полей
-- через (<$>) и (<*>), без ручного разбора Maybe.
--
-- Например:
--   parsePlayer "Alice:42" == Just (Player "Alice" 42)
--   parsePlayer "Боб:-1" == Just (Player "Боб" (-1))
--   parsePlayer ":42" == Nothing
--   parsePlayer "Bob:abc" == Nothing
--   parsePlayer "Bob:2:3" == Nothing

data Player = Player
  { playerName :: String
  , playerScore :: Integer
  }
  deriving (Eq, Show)

parseName :: String -> Maybe String
parseName = todo "1.1 name"

parseScore :: String -> Maybe Integer
parseScore = todo "1.1 score"

parsePlayer :: String -> Maybe Player
parsePlayer = todo "1.1 player"


-- 1.2. Точка на плоскости.
--
-- Операции действуют покомпонентно.
-- Завершите инстанс, затем растяните точку и сложите три точки.
-- В stretchPoint и sumPoints не разбирайте и не собирайте Point вручную.

data Point a = Point a a
  deriving (Eq, Show, Functor)

instance Applicative Point where
  pure x = Point x x

  Point _ _ <*> Point _ _ = todo "1.2 <*>"

stretchPoint :: Num a => a -> Point a -> Point a
stretchPoint = todo "1.2 stretchPoint"

sumPoints :: Num a => Point a -> Point a -> Point a -> Point a
sumPoints = todo "1.2 sumPoints"


-- 1.3. Список вычислений.
--
-- Соберите список вычислений в вычисление списка, сохранив порядок.
-- Используйте операции Applicative, без готовых sequenceA/traverse
-- и их аналогов.
--
-- На основе allResults проверьте подписку всех пользователей по их идентификаторам.
-- Just True означает, что все найдены и подписаны; Just False — все найдены,
-- но хотя бы один не подписан.
-- Пустой список даёт Just True; отсутствующий пользователь даёт Nothing,
-- даже если ранее найден пользователь без подписки.

newtype UserId = UserId Int
  deriving (Eq, Ord, Show)

data User = User { subscribed :: Bool }
  deriving (Eq, Show)

type Registry = Map UserId User

allResults :: Applicative f => [f a] -> f [a]
allResults = todo "1.3 allResults"

check :: Registry -> [UserId] -> Maybe Bool
check = todo "1.3 check"


-- 1.4. Чтение нескольких чисел.
--
-- Прочитайте n чисел из учебной консоли только операциями Applicative.
-- Сначала разберите consoleExample в Examples.hs: runMyIO возвращает
-- оставшийся мир и результат вычисления. readInt берёт одно число со входа.
--
-- При n <= 0 верните пустой список, не читая вход.
-- Предполагается достаточно входа.
-- Сохраните порядок чисел, остаток входа и уже накопленный вывод.
--
-- Например:
--   runMyIO (readInts 2) (world [2, 5, 8]) == (world [8], [2, 5])

readInts :: Int -> MyIO [Int]
readInts = todo "1.4"
