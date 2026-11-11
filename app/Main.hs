-- Пример запуска вашего summerAppMain из задания 3.3.
-- Код ввода-вывода дан готовым; разбираться в его реализации не требуется.
module Main where

import Console (World (..), runMyIO, world)
import Level3 (summerAppMain)
import System.Exit (die)
import System.IO (hFlush, stdout)
import Text.Read (readMaybe)

main :: IO ()
main = do
  putStr "Введите два числа> "
  hFlush stdout
  input <- getLine
  case map readMaybe (words input) of
    [Just x, Just y] -> do
      let (result, ()) = runMyIO summerAppMain (world [x, y])
      case worldOutput result of
        [answer] -> putStrLn $ "Результат: " ++ show answer
        output -> die $ "Ожидался один результат; ваша программа вывела: " ++ show output
    _ -> die "Введите ровно два целых числа через пробел."
