module SpecLevel1 where

import Console
import Data.Map.Strict qualified as Map
import Level1
import Test.Prelude

tests :: NamedTests
tests = nameTests 1
  [ testParsePlayer
  , testPoint
  , testAll
  , testReadInts
  ]

testParsePlayer :: Test
testParsePlayer = TestList
  [ TestCase $ assertEqual "два поля" (Just (Player "Alice" 42)) $ parsePlayer "Alice:42"
  , TestCase $ assertEqual "произвольное имя и отрицательные очки"
      (Just (Player "Боб-2" (-1))) $ parsePlayer "Боб-2:-1"
  , TestCase $ assertEqual "имя сохраняется, очки читает readMaybe"
      (Just (Player " Alice Smith " 42)) $ parsePlayer " Alice Smith : 42 "
  , TestCase $ assertEqual "Integer без ограничения Int"
      (Just (Player "Alice" bigScore)) $ parsePlayer ("Alice:" ++ show bigScore)
  , propertyToTest "имя и целые очки" $
      forAll (listOf1 (elements "aB я-2")) $ \name score ->
        parsePlayer (name ++ ":" ++ show score) === Just (Player name score)
  , TestList $ map (\raw -> TestCase $ assertEqual ("неверная запись: " ++ show raw)
      Nothing (parsePlayer raw))
      ["", "Alice", ":42", "Alice:", "Alice:abc", "Alice:2:3"]
  , TestCase $ assertEqual "parseName сохраняет имя" (Just "Алиса 2") $ parseName "Алиса 2"
  , TestCase $ assertEqual "пустое имя" Nothing $ parseName ""
  , TestCase $ assertEqual "parseScore читает Integer" (Just (-42)) $ parseScore " -42 "
  , TestCase $ assertEqual "не число" Nothing $ parseScore "abc"
  ]
  where
    bigScore = 10 ^ (30 :: Int)

instance Arbitrary a => Arbitrary (Point a) where
  arbitrary = Point <$> arbitrary <*> arbitrary

testPoint :: Test
testPoint = TestList
  [ TestCase $ assertEqual "разные функции и координаты"
      (Point 12 6) $ Point (+2) (*3) <*> Point (10 :: Int) 2
  , propertyToTest "stretch" $ \k (x :: Int) y -> stretchPoint k (Point x y) === Point (k * x) (k * y)
  , TestCase $ assertEqual "sumPoints" (Point 9 12) $ sumPoints (Point (1 :: Int) 2) (Point 3 4) (Point 5 6)
  , lawsToTest $ applicativeLaws (Proxy @Point)
  , lawsToTest $ functorLaws (Proxy @Point)
  ]

testAll :: Test
testAll = TestList
  [ TestCase $ assertEqual "пустой список" (Just ([] :: [Int])) $ allResults []
  , TestCase $ assertEqual "порядок" (Just [1, 2 :: Int]) $ allResults [Just 1, Just 2]
  , TestCase $ assertEqual "Nothing" (Nothing :: Maybe [Int]) $ allResults [Just 1, Nothing]
  , TestCase $ assertEqual "первая ошибка" (Left "a" :: Either String [Int]) $ allResults [Left "a", Left "b"]
  , TestCase $ assertEqual "все комбинации"
      [[1, 3], [1, 4], [2, 3], [2, 4 :: Int]] $ allResults [[1, 2], [3, 4]]
  , TestCase $ assertEqual "пустая команда" (Just True) $ check Map.empty []
  , TestCase $ assertEqual "все подписаны" (Just True) $ check registry [UserId 1, UserId 1]
  , TestCase $ assertEqual "False" (Just False) $ check registry [UserId 1, UserId 2]
  , TestCase $ assertEqual "False не скрывает отсутствие" Nothing $ check registry [UserId 2, UserId 0]
  ]
  where
    registry = Map.fromList [(UserId 1, User True), (UserId 2, User False)]

testReadInts :: Test
testReadInts = TestList
  [ TestCase $ assertEqual "0" (world [1], []) $ runMyIO (readInts 0) (world [1])
  , TestCase $ assertEqual "negative" (world [1], []) $ runMyIO (readInts (-2)) (world [1])
  , TestCase $ assertEqual "порядок и остаток" (world [3], [1, 2]) $ runMyIO (readInts 2) (world [1, 2, 3])
  , TestCase $ assertEqual "вывод сохранён"
      (World [8] [9], [2, 5]) $ runMyIO (readInts 2) (World [2, 5, 8] [9])
  ]
