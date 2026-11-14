-- В задаче 3.3 явно используем join; оператор (=<<) будет в следующей теме.
{- HLINT ignore "Use =<<" -}

-- | Челленджи; не входят в зачёт.
module Level3 where

import Console (MyIO, readInt, readIntMaybe, writeInt)
import Control.Monad (join)
import Data.Kind (Type)
import Level2 (Compose (..))
import MetaUtils (todo, todoImpl)


-- 3.1. Моноидальные функторы.
--
-- Моноидальный интерфейс эквивалентен Applicative.
-- Напишите инстансы для Maybe, ((->) e) и ((,) e).
-- unit не производит эффектов; (*&*) объединяет эффекты слева направо.

class Functor f => Monoidal (f :: Type -> Type) where
  unit :: f ()

  (*&*) :: f a -> f b -> f (a, b)

infixl 4 *&*

instance Monoidal Maybe where
  unit = Just ()
  mb1 *&* mb2 = case (mb1,mb2) of
    (Nothing,_) -> Nothing
    (_,Nothing) -> Nothing
    (Just x, Just y) -> Just (x, y)

instance Monoidal ((->) e) where
  unit = \_ -> ()
  f *&* g = \e -> (f e, g e)

instance Monoid e => Monoidal ((,) e) where
  unit = (mempty, ())
  (e1, x) *&* (e2, y) = (e1 <> e2, (x, y))


-- 3.2. Эквивалентность интерфейсов.
--
-- Покажите обе стороны эквивалентности интерфейсов.
-- Напишите Functor для обеих обёрток, Monoidal для ApplicativeMonoidal
-- и Applicative для MonoidalApplicative.

newtype ApplicativeMonoidal f a = ApplicativeMonoidal { runAM :: f a }
  deriving (Eq, Show)

instance Functor f => Functor (ApplicativeMonoidal f) where
  fmap g (ApplicativeMonoidal rAM) = ApplicativeMonoidal $ fmap g rAM

instance Applicative f => Monoidal (ApplicativeMonoidal f) where
  unit = ApplicativeMonoidal $ pure ()
  (ApplicativeMonoidal rAMx) *&* (ApplicativeMonoidal rAMy) = ApplicativeMonoidal $ liftA2 (,) rAMx rAMy

newtype MonoidalApplicative f a = MonoidalApplicative { runMA :: f a }
  deriving (Eq, Show)

instance Functor f => Functor (MonoidalApplicative f) where
  fmap g (MonoidalApplicative rMA) = MonoidalApplicative $ fmap g rMA

instance Monoidal f => Applicative (MonoidalApplicative f) where
  pure x = MonoidalApplicative $ fmap (\_ -> x) $ unit
  liftA2 f (MonoidalApplicative rMAx) (MonoidalApplicative rMAy) = MonoidalApplicative $ fmap (uncurry f) (rMAx *&* rMAy)


-- 3.3. Сложение на консоли.
--
-- Прочитайте два числа, сложите, запишите результат на консоль.
--
-- liftA2 (+) readInt readInt имеет тип MyIO Int, а writeInt требует Int.
-- Сначала постройте MyIO (MyIO ()), затем используйте импортированный join:
--
--   join :: Monad m => m (m a) -> m a
--
-- Для MyIO он выполняет внешнее действие, получает внутреннее действие
-- и выполняет его в мире, оставшемся после внешнего.
-- Это мост к следующей теме: результат чтения выбирает последующее действие.
--
-- Написанную программу можно запустить командой make run.

summerAppMain :: MyIO ()
summerAppMain = join myIOmyIO
  where
    myIOmyIO = writeInt <$> liftA2 (+) readInt readInt


-- 3.4. Композиция MyIO и Maybe.
--
-- Прочитайте n чисел и сложите.
-- При недостатке чисел верните Nothing; при n <= 0 — Just 0 без чтения.
-- Используйте свой Compose из 2.3, readIntMaybe и операции Applicative,
-- без Monad/join.
--
-- readIntMaybe потребляет одно число и возвращает Just с ним; на пустом входе
-- возвращает Nothing. Сохраните остаток входа и уже накопленный вывод.

type App = Compose MyIO Maybe

sumN :: Int -> App Int
sumN n
  | n <= 0 = pure 0
  | otherwise = liftA2 (+) (Compose readIntMaybe) (sumN (n - 1))
