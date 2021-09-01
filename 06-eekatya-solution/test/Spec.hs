import Control.Monad (forM)
import Data.List (sort)
import Data.Maybe (catMaybes, isNothing)
import Lib
import Test.HUnit
import Test.QuickCheck (chatty, isSuccess, output, quickCheckWithResult, stdArgs)

qcArgs = stdArgs {chatty = False}

quickTestToTestCase descr test = TestCase $ do
  result <- test
  let message = descr ++ ", но\n" ++ output result
  if isSuccess result
    then pure ()
    else assertBool message False

task1Test =
  TestList
    [ TestCase $ assertEqual "fmap на дереве из одной вершины, " (Node (9 :: Int) []) (task1 (^ 2) $ Node 3 [])
    ]

task2Test = TestList []

task3Test = TestList []

task4Test =
  TestList
    [ TestCase $ assertEqual "пустой развёрнутый список, " "[]" (task4 (ReversedList ([] :: [Char]))),
      TestCase $
        assertEqual
          "одноэлементный развёрнутый список, "
          "[1]"
          (task4 (ReversedList [1 :: Int])),
      TestCase $
        assertEqual
          "трёхэлементный развёрнутый список, "
          "[1,4,5]"
          (task4 (ReversedList [5, 4, 1 :: Int])),
      quickTestToTestCase "список выглядит так же, как развёрнутый обычный" $
        quickCheckWithResult qcArgs (rule :: [Int] -> Bool),
      quickTestToTestCase "список ()" $
        quickCheckWithResult qcArgs (rule :: [()] -> Bool)
    ]
  where
    rule :: (Show a) => [a] -> Bool
    rule lst = task4 (ReversedList lst) == show (reverse lst)

churchToInteger :: Church -> Integer
churchToInteger (Church a) = a succ 0

ch0 = Church $ \s z -> z

ch1 = Church $ \s z -> s z

ch2 = Church $ \s z -> s (s z)

ch3 = Church $ \s z -> s (s (s z))

ch4 = Church $ \s z -> s (s (s (s z)))

ch5 = Church $ \s z -> s (s (s (s (s z))))

task5Test =
  TestList
    [ TestCase $ assertEqual "succ 0, " 1 (churchToInteger $ mySucc task5 ch0),
      TestCase $ assertEqual "succ 1, " 2 (churchToInteger $ mySucc task5 ch1),
      TestCase $ assertEqual "succ 2, " 3 (churchToInteger $ mySucc task5 ch2),
      TestCase $ assertEqual "succ 3, " 4 (churchToInteger $ mySucc task5 ch3),
      TestCase $ assertEqual "succ 4, " 5 (churchToInteger $ mySucc task5 ch4),
      TestCase $ assertEqual "succ 5, " 6 (churchToInteger $ mySucc task5 ch5),
      TestCase $ assertEqual "pred 1, " 0 (churchToInteger $ myPred task5 ch1),
      TestCase $ assertEqual "pred 2, " 1 (churchToInteger $ myPred task5 ch2),
      TestCase $ assertEqual "pred 3, " 2 (churchToInteger $ myPred task5 ch3),
      TestCase $ assertEqual "pred 4, " 3 (churchToInteger $ myPred task5 ch4),
      TestCase $ assertEqual "pred 5, " 4 (churchToInteger $ myPred task5 ch5),
      TestCase $ assertEqual "fromEnum 0, " 0 (myFromEnum task5 ch0),
      TestCase $ assertEqual "fromEnum 1, " 1 (myFromEnum task5 ch1),
      TestCase $ assertEqual "fromEnum 2, " 2 (myFromEnum task5 ch2),
      TestCase $ assertEqual "fromEnum 3, " 3 (myFromEnum task5 ch3),
      TestCase $ assertEqual "fromEnum 4, " 4 (myFromEnum task5 ch4),
      TestCase $ assertEqual "fromEnum 5, " 5 (myFromEnum task5 ch5),
      TestCase $ assertEqual "toEnum 3, " 3 (churchToInteger $ myToEnum task5 3),
      quickTestToTestCase "toEnum" $
        quickCheckWithResult qcArgs $
          \n -> n < 0 || n == (fromInteger $ churchToInteger $ myToEnum task5 n)
    ]

task6Test =
  TestList
    [ TestCase $
        assertEqual
          "2 * 3 + 14 - 3"
          17
          ( churchToInteger $
              myMinus
                t
                ( myPlus
                    t
                    (myMult t ch2 ch3)
                    (myFromInteger t 14)
                )
                ch3
          ),
      TestCase $ assertEqual "signum 0" 0 (churchToInteger $ mySignum t ch0),
      TestCase $ assertEqual "signum 1" 1 (churchToInteger $ mySignum t ch1),
      TestCase $ assertEqual "signum 2" 1 (churchToInteger $ mySignum t ch2),
      TestCase $ assertEqual "signum 5" 1 (churchToInteger $ mySignum t ch5),
      TestCase $ assertEqual "abs 0" 0 (churchToInteger $ myAbs t ch0),
      TestCase $ assertEqual "abs 1" 1 (churchToInteger $ myAbs t ch1),
      TestCase $ assertEqual "abs 2" 2 (churchToInteger $ myAbs t ch2),
      TestCase $ assertEqual "abs 5" 5 (churchToInteger $ myAbs t ch5),
      TestCase $ assertEqual "0 * 0" 0 (churchToInteger $ myMult t ch0 ch0),
      TestCase $ assertEqual "0 * 1" 0 (churchToInteger $ myMult t ch0 ch1),
      TestCase $ assertEqual "0 * 2" 0 (churchToInteger $ myMult t ch0 ch2),
      TestCase $ assertEqual "1 * 0" 0 (churchToInteger $ myMult t ch1 ch0),
      TestCase $ assertEqual "1 * 1" 1 (churchToInteger $ myMult t ch1 ch1),
      TestCase $ assertEqual "1 * 2" 2 (churchToInteger $ myMult t ch1 ch2),
      TestCase $ assertEqual "2 * 0" 0 (churchToInteger $ myMult t ch2 ch0),
      TestCase $ assertEqual "2 * 1" 2 (churchToInteger $ myMult t ch2 ch1),
      TestCase $ assertEqual "2 * 2" 4 (churchToInteger $ myMult t ch2 ch2),
      TestCase $ assertEqual "0 * 0" 0 (churchToInteger $ myMinus t ch0 ch0),
      TestCase $ assertEqual "1 * 0" 1 (churchToInteger $ myMinus t ch1 ch0),
      TestCase $ assertEqual "1 * 1" 0 (churchToInteger $ myMinus t ch1 ch1),
      TestCase $ assertEqual "2 * 0" 2 (churchToInteger $ myMinus t ch2 ch0),
      TestCase $ assertEqual "2 * 1" 1 (churchToInteger $ myMinus t ch2 ch1),
      TestCase $ assertEqual "2 * 2" 0 (churchToInteger $ myMinus t ch2 ch2),
      TestCase $ assertEqual "0 + 0" 0 (churchToInteger $ myPlus t ch0 ch0),
      TestCase $ assertEqual "0 + 1" 1 (churchToInteger $ myPlus t ch0 ch1),
      TestCase $ assertEqual "0 + 2" 2 (churchToInteger $ myPlus t ch0 ch2),
      TestCase $ assertEqual "1 + 0" 1 (churchToInteger $ myPlus t ch1 ch0),
      TestCase $ assertEqual "1 + 1" 2 (churchToInteger $ myPlus t ch1 ch1),
      TestCase $ assertEqual "1 + 2" 3 (churchToInteger $ myPlus t ch1 ch2),
      TestCase $ assertEqual "2 + 0" 2 (churchToInteger $ myPlus t ch2 ch0),
      TestCase $ assertEqual "2 + 1" 3 (churchToInteger $ myPlus t ch2 ch1),
      TestCase $ assertEqual "2 + 2" 4 (churchToInteger $ myPlus t ch2 ch2)
    ]
  where
    t = task6

task7Test =
  TestList
    [ TestCase $
        assertEqual "пустой список" ([] :: [(Char, String)]) $
          let (PairReaderList lst) =
                task7 (++ "x") $
                  PairReaderList []
           in map ($ 3) lst,
      TestCase $
        assertEqual "какая-то белиберда" [(3, "3x"), (2, "yx"), (3, "33x")] $
          let (PairReaderList lst) =
                task7 (++ "x") $
                  PairReaderList [\s -> (3, show s), const (2, "y"), \x -> (x, show x ++ show x)]
           in map ($ 3) lst
    ]

task8Test =
  TestList
    [ TestCase $
        assertEqual
          "functionA"
          (Just ('c', [Just 10, Nothing, Just 12]))
          (task8 (+ 5) (Just ('c', [Just 5, Nothing, Just 7]))),
      TestCase $
        assertEqual
          "functionB"
          ('c', [[Just 16], [Nothing, Nothing, Just 4], [Nothing]])
          (task8 (^ 2) ('c', [[Just 4], [Nothing, Nothing, Just 2], [Nothing]]))
    ]

task9Test =
  TestList
    [ quickTestToTestCase "строка отсортирована" $
        quickCheckWithResult qcArgs $ \s -> sort (s :: String) == task9 s,
      quickTestToTestCase "список отсортирован" $
        quickCheckWithResult qcArgs $ \s -> sort (s :: [Int]) == task9 s
    ]

task10Test =
  TestList
    [ TestCase $ assertEqual "sort (-1042)" (-124) (runSortableIntegral $ task10 $ MkSortableIntegral (-1042)),
      TestCase $ assertEqual "sort 0" 0 (runSortableIntegral $ task10 $ MkSortableIntegral 0),
      TestCase $ assertEqual "sort 523" 235 (runSortableIntegral $ task10 $ MkSortableIntegral 523),
      TestCase $ assertEqual "sort 5023987654321" 122334556789 (runSortableIntegral $ task10 $ MkSortableIntegral 5023987654321),
      TestCase $ assertEqual "sort (2^70 + 3)" 1111112233445677789 (runSortableIntegral $ task10 $ MkSortableIntegral (2 ^ 70 + 3)),
      TestCase $
        assertEqual
          "sort (-2^70 + 3)"
          (-1111111223344567789)
          (runSortableIntegral $ task10 $ MkSortableIntegral (-2 ^ 70 + 3 :: Integer))
    ]

tests =
  TestList
    [ TestList [],
      task1Test,
      task2Test,
      task3Test,
      task4Test,
      task5Test,
      task6Test,
      task7Test,
      task8Test,
      task9Test,
      task10Test
    ]

main = runTestTT tests
