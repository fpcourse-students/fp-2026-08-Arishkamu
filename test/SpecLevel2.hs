module SpecLevel2 where

import Data.List (find)
import Examples qualified as E
import Level1
import Level2
import Test.Prelude

tests :: NamedTests
tests = nameTests 2
  [ testPointN
  , testIndexed
  , testCompose
  , testBinary
  , testBestPlayer
  ]

testPointN :: Test
testPointN = TestList
  [ TestCase $ assertBool "контрпример Identity" $
      E.badIdentity identityCounterexample /= identityCounterexample
  , TestCase $ assertEqual "длинное pure" (replicate 20 (7 :: Int)) $ take 20 $ getPointN (pure 7)
  , propertyToTest "Identity" $ \(xs :: [Int]) -> getPointN (pure id <*> PointN xs) === xs
  , propertyToTest "Homomorphism (префикс)" $ \(Fun _ f :: Fun Int Int) x ->
      take 20 (getPointN (pure f <*> pure x)) === take 20 (getPointN (pure (f x)))
  , propertyToTest "Interchange" $ \(fs :: [Fun Int Int]) x ->
      let u = PointN (map applyFun fs) in
      getPointN (u <*> pure x) === getPointN (pure ($ x) <*> u)
  , propertyToTest "Composition" $ \(fs :: [Fun Int Int]) (gs :: [Fun Int Int]) xs ->
      let u = PointN (map applyFun fs) in
      let v = PointN (map applyFun gs) in
      let w = PointN xs in
      getPointN (pure (.) <*> u <*> v <*> w) === getPointN (u <*> (v <*> w))
  , propertyToTest "zip, не декартово произведение" $ \(xs :: [Int]) ys ->
      getPointN ((+) <$> PointN xs <*> PointN ys) === zipWith (+) xs ys
  ]

testIndexed :: Test
testIndexed = TestList
  [ propertyToTest "общий индекс, применение, fmap" $ \(Fun _ f :: Fun Bool Int) (Fun _ g :: Fun Bool Int) i ->
      let p = PointI f in
      let q = PointI g in
      conjoin
        [ at (show <$> p) i === show (f i)
        , at ((+) <$> p <*> q) i === f i + g i
        ]
  , propertyToTest "Identity" $ \(Fun _ f :: Fun Bool Int) i -> at (pure id <*> PointI f) i === f i
  , TestCase $ assertEqual "PointI pure" (7 :: Int) $ at (pure 7) False
  ]

instance Arbitrary (f (g a)) => Arbitrary (Compose f g a) where
  arbitrary = Compose <$> arbitrary

testCompose :: Test
testCompose = TestList
  [ TestCase $ assertEqual "два fmap"
      (Compose [Just "2", Nothing]) $ fmap show (Compose [Just (2 :: Int), Nothing])
  , TestCase $ assertEqual "наружный список, внутренний Maybe"
      (Compose [Just 11, Just 21, Nothing, Nothing]) $
      (+) <$> Compose [Just (1 :: Int), Nothing] <*> Compose [Just 10, Just 20]
  , TestCase $ assertEqual "ошибка внутри"
      (Compose (Just (Left "err")) :: Compose Maybe (Either String) Int) $
      (+) <$> Compose (Just (Left "err")) <*> Compose (Just (Right 2))
  , lawsToTest $ functorLaws (Proxy @(Compose [] Maybe))
  , lawsToTest $ applicativeLaws (Proxy @(Compose [] Maybe))
  ]

testBinary :: Test
testBinary = TestList
  [ TestCase $ assertBool "одинаковые" $ sameBinary (&&) (&&)
  , TestCase $ assertBool "разные" $ not $ sameBinary (&&) (||)
  , TestCase $ assertBool "ловушка диагонального перебора" $
      not $ sameBinary (==) (\(_ :: Bool) (_ :: Bool) -> True)
  , TestCase $ assertBool "разные области" $
      sameBinary (\b o -> if b then o else EQ) (\b o -> if b then o else EQ)
  , TestCase $ assertBool "вне диагонали Ordering" $
      not $ sameBinary (\(a :: Ordering) b -> a == b) (\a b -> a == b || (a == LT && b == GT))
  ]

testBestPlayer :: Test
testBestPlayer = TestList
  [ TestCase $ assertEqual "пустой список" Nothing $ bestPlayer []
  , TestCase $ assertEqual "один игрок" (Just (Player "Solo" (-5))) $
      bestPlayer [Player "Solo" (-5)]
  , TestCase $ assertEqual "максимум в середине" (Just (Player "Bob" 7)) $
      bestPlayer [Player "Alice" 2, Player "Bob" 7, Player "Carol" 3]
  , TestCase $ assertEqual "первый при равенстве" (Just (Player "Bob" 7)) $
      bestPlayer [Player "Alice" 2, Player "Bob" 7, Player "Carol" 7, Player "Dave" 1]
  , TestCase $ assertEqual "все очки отрицательны" (Just (Player "Carol" (-1))) $
      bestPlayer [Player "Alice" (-8), Player "Bob" (-3), Player "Carol" (-1)]
  , propertyToTest "первый игрок с максимальными очками" $ \(NonEmpty scores) ->
      let players = zipWith (Player . show) [1 :: Int ..] scores in
      bestPlayer players === find ((== maximum scores) . playerScore) players
  ]
