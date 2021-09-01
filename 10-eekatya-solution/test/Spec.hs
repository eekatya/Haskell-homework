{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

import Control.Arrow ((&&&))
import Control.Monad (join)
import Data.Function (on)
import Data.List (group, sort)
import Generic.Random (genericArbitrary', genericArbitraryU, (%))
import Lib
import Test.HUnit
import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

tests =
  TestList
    [ TestList [],
      task1Test,
      task2Test,
      task3Test,
      task4Test,
      task5Test,
      task6Test,
      task7Test
    ]

main = runTestTT tests

qcArgs = stdArgs {maxSize = 20}

quickTestToTestCase descr test = TestCase $ do
  result <- test
  let message = "ожидается, что " ++ descr ++ ", но\n" ++ output result
  if isSuccess result
    then pure ()
    else assertBool message False

-- Task 1 ---------------------------------------------------------------------

instance (CoArbitrary e, Arbitrary a) => Arbitrary (FilterList e a) where
  arbitrary = (\i l -> FilterList (take i l)) <$> getSize <*> arbitrary

instance Show a => Show (FilterList Bool a) where
  show xs =
    "[True -> " ++ show (filterListToList True xs) ++ ", "
      ++ "False -> "
      ++ show (filterListToList False xs)

instance (Show a, Ord a, Eq a) => EqProp (FilterList Bool a) where
  xs =-= ys =
    sort (filterListToList True xs) === sort (filterListToList True ys)
      .&. sort (filterListToList False xs) === sort (filterListToList False ys)

task1Test =
  TestList $
    [ TestCase $
        checkBatch qcArgs $
          applicative (undefined :: FilterList Bool (Int, Double, Bool)),
      TestCase $
        checkBatch qcArgs $
          monad (undefined :: FilterList Bool (Int, Double, Bool)),
      TestCase $
        assertEqual "пример из задания, " "nnx" $
          sort $
            filterListToList 4 $
              join $
                FilterList
                  [ ( (> 2),
                      FilterList
                        [ ((< 5), 'x'),
                          ((< 3), 'y'),
                          ((< 2), 'z')
                        ]
                    ),
                    ( odd,
                      FilterList
                        [ (\e -> e `mod` 3 == 0, 'a'),
                          ((== 4), 'b'),
                          ((== 5), 'c')
                        ]
                    ),
                    ( \e -> abs e <= 100,
                      FilterList
                        [ (\e -> e `mod` 3 == 0, 'm'),
                          ((== 4), 'n'),
                          ((\e -> abs e <= 10), 'n')
                        ]
                    )
                  ]
    ]

-- Task 2 ---------------------------------------------------------------------

instance Arbitrary a => Arbitrary (Point a) where
  arbitrary = genericArbitraryU

instance Eq a => EqProp (Point a) where
  (=-=) = eq

task2Test =
  TestList $
    [ TestCase $ quickBatch $ applicative (undefined :: Point (Int, Double, Bool)),
      TestCase $ quickBatch $ monad (undefined :: Point (Int, Double, Bool)),
      TestCase $
        assertEqual "пример из задания, " (Point 13 16) $ do
          a <- Point 3 4
          b <- Point 10 12
          pure (a + b)
    ]

-- Task 3 ---------------------------------------------------------------------

task3Test =
  TestList $
    [ quickTestToTestCase "проверка в монаде списка, " $
        quickCheckWithResult (qcArgs {chatty = False}) $
          property $
            forAll (arbitrary :: Gen ([[[[Int]]]], [[Char]])) $
              \(a, b) -> task3 a b === term a b,
      quickTestToTestCase "проверка в монаде Maybe, " $
        quickCheckWithResult (qcArgs {chatty = False}) $
          property $
            forAll (arbitrary :: Gen (Maybe [Maybe (Maybe Int)], Maybe [Char])) $
              \(a, b) -> task3 a b === term a b
    ]
  where
    term a b = do
      a
      b
      if 3 < 5
        then do
          a
          return 4
        else return 5
      x <- a
      let y = 5
      [x, y] <- b
      [x, y, z] <- a
      x <- x
      x
      pure x

-- Task 4 ---------------------------------------------------------------------

task4Test =
  TestList $
    [ TestCase $
        assertEqual "правильный результат, " task45Result $
          take (length task45Result) task4
    ]

-- Task 5 ---------------------------------------------------------------------

task5Test =
  TestList $
    [ TestCase $
        assertEqual "правильный результат, " task45Result $
          take (length task45Result) task5
    ]

task45Result =
  [ (5, 4, 3),
    (10, 8, 6),
    (13, 12, 5),
    (15, 12, 9),
    (17, 15, 8),
    (20, 16, 12),
    (25, 20, 15),
    (25, 24, 7),
    (26, 24, 10),
    (29, 21, 20),
    (30, 24, 18),
    (34, 30, 16),
    (35, 28, 21),
    (37, 35, 12),
    (39, 36, 15),
    (40, 32, 24),
    (41, 40, 9),
    (45, 36, 27),
    (50, 40, 30),
    (50, 48, 14),
    (51, 45, 24),
    (52, 48, 20),
    (53, 45, 28),
    (55, 44, 33),
    (58, 42, 40),
    (60, 48, 36),
    (61, 60, 11),
    (65, 52, 39),
    (65, 56, 33),
    (65, 60, 25),
    (65, 63, 16),
    (68, 60, 32),
    (70, 56, 42),
    (73, 55, 48),
    (74, 70, 24),
    (75, 60, 45),
    (75, 72, 21),
    (78, 72, 30),
    (80, 64, 48),
    (82, 80, 18),
    (85, 68, 51),
    (85, 75, 40),
    (85, 77, 36),
    (85, 84, 13),
    (87, 63, 60),
    (89, 80, 39),
    (90, 72, 54),
    (91, 84, 35),
    (95, 76, 57),
    (97, 72, 65),
    (100, 80, 60),
    (100, 96, 28),
    (101, 99, 20),
    (102, 90, 48),
    (104, 96, 40),
    (105, 84, 63),
    (106, 90, 56),
    (109, 91, 60),
    (110, 88, 66),
    (111, 105, 36),
    (113, 112, 15),
    (115, 92, 69),
    (116, 84, 80),
    (117, 108, 45),
    (119, 105, 56),
    (120, 96, 72),
    (122, 120, 22),
    (123, 120, 27),
    (125, 100, 75),
    (125, 117, 44),
    (125, 120, 35),
    (130, 104, 78),
    (130, 112, 66),
    (130, 120, 50),
    (130, 126, 32),
    (135, 108, 81),
    (136, 120, 64),
    (137, 105, 88),
    (140, 112, 84),
    (143, 132, 55),
    (145, 105, 100),
    (145, 116, 87),
    (145, 143, 24),
    (145, 144, 17),
    (146, 110, 96),
    (148, 140, 48),
    (149, 140, 51),
    (150, 120, 90),
    (150, 144, 42),
    (153, 135, 72),
    (155, 124, 93),
    (156, 144, 60),
    (157, 132, 85),
    (159, 135, 84),
    (160, 128, 96),
    (164, 160, 36),
    (165, 132, 99),
    (169, 120, 119),
    (169, 156, 65),
    (170, 136, 102),
    (170, 150, 80),
    (170, 154, 72),
    (170, 168, 26),
    (173, 165, 52),
    (174, 126, 120),
    (175, 140, 105),
    (175, 168, 49),
    (178, 160, 78),
    (180, 144, 108),
    (181, 180, 19),
    (182, 168, 70),
    (183, 180, 33),
    (185, 148, 111),
    (185, 153, 104),
    (185, 175, 60),
    (185, 176, 57),
    (187, 165, 88),
    (190, 152, 114),
    (193, 168, 95),
    (194, 144, 130),
    (195, 156, 117),
    (195, 168, 99),
    (195, 180, 75),
    (195, 189, 48),
    (197, 195, 28),
    (200, 160, 120),
    (200, 192, 56),
    (202, 198, 40),
    (203, 147, 140),
    (204, 180, 96),
    (205, 156, 133),
    (205, 164, 123),
    (205, 187, 84),
    (205, 200, 45),
    (208, 192, 80),
    (210, 168, 126),
    (212, 180, 112),
    (215, 172, 129),
    (218, 182, 120),
    (219, 165, 144),
    (220, 176, 132),
    (221, 171, 140),
    (221, 195, 104),
    (221, 204, 85),
    (221, 220, 21),
    (222, 210, 72),
    (225, 180, 135),
    (225, 216, 63),
    (226, 224, 30),
    (229, 221, 60),
    (230, 184, 138),
    (232, 168, 160),
    (233, 208, 105),
    (234, 216, 90),
    (235, 188, 141),
    (238, 210, 112),
    (240, 192, 144),
    (241, 209, 120),
    (244, 240, 44),
    (245, 196, 147),
    (246, 240, 54),
    (247, 228, 95),
    (250, 200, 150),
    (250, 234, 88),
    (250, 240, 70),
    (255, 204, 153),
    (255, 225, 120),
    (255, 231, 108),
    (255, 252, 39),
    (257, 255, 32),
    (259, 245, 84),
    (260, 208, 156),
    (260, 224, 132),
    (260, 240, 100),
    (260, 252, 64)
  ]

-- Task 6 ---------------------------------------------------------------------

task6Test =
  TestList $
    [ quickTestToTestCase "числа правильно преумножаются" $
        quickCheckWithResult (qcArgs {chatty = False}) $
          property $
            forAll (arbitrary :: Gen [Int]) $ \xs' ->
              let groups = (length &&& head) <$> group (task6 xs)
                  xs = map abs xs'
               in label "не должны встречаться нули"
                      (filter (== 0) (task6 xs) === [])
                  .&. label "подряд должно идти кратное n число n"
                      (conjoin ((\(l, h) -> l `mod` h === 0) <$> groups))
                  .&. label "попытка восстановить исходный список"
                      (filter (/= 0) xs ===
                       (groups >>= (\(l, h) -> replicate (l `div` h) h)))
    ]

-- Task 7 ---------------------------------------------------------------------

task7Test =
  TestList $
    [ TestCase $ assertDoubleEq "task7 1, " 1 $ task7 1,
      TestCase $ assertDoubleEq "task7 2, " 0.75 $ task7 2,
      TestCase $ assertDoubleEq "task7 3, " 0.4444444444 $ task7 3,
      TestCase $ assertDoubleEq "task7 4, " 0.4453125 $ task7 4,
      TestCase $ assertDoubleEq "task7 5, " 0.3766400000 $ task7 5,
      TestCase $ assertDoubleEq "task7 6, " 0.3556884431 $ task7 6
    ]
  where
    assertDoubleEq str a b =
      assertBool str (abs (a - b) < 0.000001)
