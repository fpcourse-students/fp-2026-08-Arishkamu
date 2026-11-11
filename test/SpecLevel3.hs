module SpecLevel3 where

import Console
import Level2 (Compose (..))
import Level3
import Test.Prelude

tests :: NamedTests
tests = nameTests 3
  [ testMonoidal
  , testConversions
  , testSummer
  , testSum
  ]

testMonoidal :: Test
testMonoidal = TestList
  [ TestCase $ assertEqual "Maybe unit" (Just ()) (unit :: Maybe ())
  , TestCase $ assertEqual "Maybe pair" (Just (1, True)) ((Just (1 :: Int)) *&* Just True)
  , TestCase $ assertEqual "Maybe failure" (Nothing :: Maybe (Int, Bool)) (Just 1 *&* Nothing)
  , TestCase $ assertEqual "reader unit" () ((unit :: Bool -> ()) True)
  , TestCase $ assertEqual "reader same input" (4, 6) (((+1) *&* (*2)) (3 :: Int))
  , TestCase $ assertEqual "writer unit" ("", ()) (unit :: (String, ()))
  , TestCase $ assertEqual "writer order" ("ab", (1, True)) (("a", 1 :: Int) *&* ("b", True))
  ]

instance Arbitrary (f a) => Arbitrary (MonoidalApplicative f a) where
  arbitrary = MonoidalApplicative <$> arbitrary

instance Arbitrary (f a) => Arbitrary (ApplicativeMonoidal f a) where
  arbitrary = ApplicativeMonoidal <$> arbitrary

testConversions :: Test
testConversions = TestList
  [ TestCase $ assertEqual "AM unit" (Just ()) $ runAM (unit :: ApplicativeMonoidal Maybe ())
  , TestCase $ assertEqual "AM product"
      (Just (1, True)) $ runAM (ApplicativeMonoidal (Just (1 :: Int)) *&* ApplicativeMonoidal (Just True))
  , TestCase $ assertEqual "AM annotations"
      ("ab", (1, True)) $ runAM (ApplicativeMonoidal ("a", 1 :: Int) *&* ApplicativeMonoidal ("b", True))
  , lawsToTest $ applicativeLaws (Proxy @(MonoidalApplicative Maybe))
  , lawsToTest $ applicativeLaws (Proxy @(MonoidalApplicative ((,) String)))
  , lawsToTest $ functorLaws (Proxy @(ApplicativeMonoidal Maybe))
  , lawsToTest $ functorLaws (Proxy @(MonoidalApplicative Maybe))
  ]

testSummer :: Test
testSummer = TestList
  [ TestCase $ assertEqual "прочитать два, записать одно, сохранить хвост"
      (World [8] [7], ()) $ runMyIO summerAppMain (world [2, 5, 8])
  , TestCase $ assertEqual "дописать вывод" (World [] [9, -1], ()) $ runMyIO summerAppMain (World [-3, 2] [9])
  ]

testSum :: Test
testSum = TestList
  [ TestCase $ assertEqual "0" (world [9], Just 0) $ runMyIO (getCompose (sumN 0)) (world [9])
  , TestCase $ assertEqual "negative" (world [9], Just 0) $ runMyIO (getCompose (sumN (-3))) (world [9])
  , TestCase $ assertEqual "сумма и остаток"
      (world [7], Just 3) $ runMyIO (getCompose (sumN 2)) (world [1, 2, 7])
  , TestCase $ assertEqual "ровно достаточно"
      (world [], Just 3) $ runMyIO (getCompose (sumN 2)) (world [1, 2])
  , TestCase $ assertEqual "недостаток входа"
      (world [], Nothing) $ runMyIO (getCompose (sumN 3)) (world [1, 2])
  , TestCase $ assertEqual "пустой вход" (world [], Nothing) $ runMyIO (getCompose (sumN 1)) (world [])
  , TestCase $ assertEqual "вывод сохранён"
      (World [7] [9], Just 3) $ runMyIO (getCompose (sumN 2)) (World [1, 2, 7] [9])
  ]
