-- Сохраняем (<$>) в примере аппликатива пары.
{- HLINT ignore "Using <$> on tuple" -}

-- | Данный код для чтения; менять его не требуется.
module Examples where

import Control.Applicative (ZipList (..))
import Console (World, readInt, runMyIO, world)

cartesian :: [Int]
cartesian = (+) <$> [1, 2] <*> [10, 20]

zipped :: [Int]
zipped = getZipList $ (+) <$> ZipList [1, 2] <*> ZipList [10, 20]

logged :: (String, Int)
logged = (+) <$> ("left;", 2) <*> ("right;", 3)

-- Правдоподобная, но неверная реализация pure для покомпонентного аппликатива.
badIdentity :: [a] -> [a]
badIdentity = zipWith ($) [id]

-- К задаче 1.4. Это учебная модель консоли, которую можно запускать в GHCi.
-- world [2, 5, 8] задаёт вход и пустой вывод. Каждое readInt берёт следующее число;
-- Applicative выполняет чтения слева направо. runMyIO возвращает пару:
--   (World {worldInput = [8], worldOutput = []}, (2, 5))
-- Два числа стали результатом, третье осталось во входе; вывода не было.
consoleExample :: (World, (Int, Int))
consoleExample = runMyIO ((,) <$> readInt <*> readInt) (world [2, 5, 8])
