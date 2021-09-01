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
  TestList $
    [ quickTestToTestCase "исключение к полугруппе" $
        quickCheckWithResult qcArgs rule1,
      quickTestToTestCase "операция полугруппы" $
        quickCheckWithResult qcArgs rule2,
      quickTestToTestCase "контрпример к моноиду" $
        quickCheckWithResult qcArgs rule3,
      quickTestToTestCase "единица моноида" $
        quickCheckWithResult qcArgs rule4
    ]
  where
    -- |Проверяем, правда ли, что на данных числах gcd не ассоциативен.
    rule1 :: () -> Bool
    rule1 _ = case task1 of
      Left (a', b', c') ->
        ((a' `gcd` b') `gcd` c') /= (a' `gcd` (b' `gcd` c'))
      _ -> True
    -- |Проверяем, выполняется ли ассоциативность операции.
    rule2 :: Int -> Int -> Int -> Bool
    rule2 a b c =
      let (ga, gb, gc) = (Gcd a, Gcd b, Gcd c)
       in case task1 of
            Right (op, r) -> ((ga `op` gb) `op` gc) == (ga `op` (gb `op` gc))
            _ -> True
    -- |Проверяем, есть ли какое-то такое число, что gcd с ним контрпримера
    -- равно контрпримеру.
    rule3 :: Int -> Bool
    rule3 a = case task1 of
      Right (op, Left a') ->
        Gcd a' `op` Gcd a /= Gcd a' || Gcd a `op` Gcd a' /= Gcd a'
      _ -> True
    -- |Убеждаемся, что единица действительно является единицей.
    rule4 :: Int -> Bool
    rule4 a = case task1 of
      Right (op, Right ga') ->
        ga' `op` Gcd a == Gcd a && Gcd a `op` ga' == Gcd a
      _ -> True


-- Ваши тесты идут сюда:
task2Test =
  TestList
    []

task4Test =
  TestList
    [TestCase $ assertEqual "пример из условия, "
                            [1 .. 10] (task4 (:[]) testTree)]
  where
    testTree =
      Node
        1
        (Node 4 Leaf Leaf)
        (Node 2 (Node 5 (Node 8 Leaf Leaf)
                        (Node 6 (Node 9 (Node 10 Leaf Leaf) Leaf) Leaf))
                (Node 3 (Node 7 Leaf Leaf) Leaf))

task5Test =
  TestList
    [quickTestToTestCase "аналогично `length . filter p`" $
        quickCheckWithResult qcArgs rule]
    where rule :: [Int] -> Bool
          rule xs = task5 even xs == length (filter even xs)

task6Test =
  TestList
    [quickTestToTestCase "аналогично простой версии" $
        quickCheckWithResult qcArgs rule]
    where rule :: Either (Int, Int) String -> Bool
          rule e = task6 e == case e of
                                Left (a, b) -> a + b
                                Right s -> length s

task7Test = TestList [
             TestCase $ assertEqual "проходит по всем элементам, "
               "abcdef" (task7 id testHeap),
             TestCase $ assertEqual "не заглядывает в элементы, "
               6 (length $ task7 (:[]) $ (\a -> (undefined :: Int)) <$> testHeap),
             TestCase $ assertEqual "не заходит в лишние ветки, "
               "abc" (take 3 $ task7 id testHeap')
            ]
          where testHeap' = HeapNode
                              0
                              ['a']
                              ( HeapNode
                                  1
                                  ['b']
                                  (HeapNode 3 undefined undefined undefined)
                                  (HeapNode 2 ['c'] HeapLeaf HeapLeaf)
                              )
                              (HeapNode 1000 ['d'] undefined undefined)
                testHeap = HeapNode 0 "a"
                             (HeapNode 1 "b"
                               (HeapNode 4 "e" HeapLeaf HeapLeaf)
                               (HeapNode 3 "d" HeapLeaf HeapLeaf)
                             )
                             (HeapNode 2 "c"
                               (HeapNode 5 "f"
                                 HeapLeaf
                                 HeapLeaf)
                               HeapLeaf)


task8Test = TestList
    [quickTestToTestCase "аналогично map" $
        quickCheckWithResult qcArgs rule,
     TestCase $ assertEqual "работает на бесконечных списках"
        [1, 2, 3, 4] (take 4 $ task8 id ([1..4] ++ undefined))]
    where rule :: [Int] -> Bool
          rule xs = map (^2) xs == task8 (^2) xs

task9Test = TestList
    [quickTestToTestCase "аналогично `takeWhile p . iterate f`" $
        quickCheckWithResult qcArgs rule,
     TestCase $ assertEqual "выдаёт частичный результат"
        [1, 2, 3, 4] (take 4 $ task9 (const True) (\x -> if x == 4 then undefined else succ x) 1)
    ]
    where rule :: Int -> Bool
          rule i = takeWhile p (iterate (*3) i) == task9 p (*3) i
          p x = odd x && abs x < 1000

task10Test = TestList
    [quickTestToTestCase "аналогично `(take n xs, drop n xs)`" $
        quickCheckWithResult qcArgs rule,
     TestCase $ assertEqual "работает на бесконечных списках"
        [1 .. 4] (fst $ task10 4 [1..])]
    where rule :: Int -> [Int] -> Bool
          rule n xs = (take n xs, drop n xs) == task10 n xs

task11Test = TestList
    [quickTestToTestCase "всё равно что разбить спискок на два, сделать map и склеить назад" $
        quickCheckWithResult qcArgs rule,
     TestCase $ assertEqual "работает на бесконечных списках"
        [-99,6,-97,12,-95,18,-93,24,-91,30]
        (take 10 $ task11 (+ (-100)) (*3) $ [1..10] ++ undefined)]
    where rule :: [Int] -> Bool
          rule xs = task11 (+ (-100)) (^2) xs ==
                    merge (everySecond $ map (+ (-100)) $ xs)
                          (everySecond $ map (^2) $ drop 1 xs)
          merge [] ys = ys
          merge xs [] = xs
          merge (x:xs) (y:ys) = x:y:merge xs ys
          everySecond [] = []
          everySecond [x] = [x]
          everySecond (x:y:xs) = x:everySecond xs

task12Test = TestList
    [quickTestToTestCase "аналогично `elem`" $
        quickCheckWithResult qcArgs rule,
     TestCase $ assertEqual "работает на бесконечных списках"
         True
         (task12 4 $ [1..4] ++ undefined)]
    where rule :: Int -> [Int] -> Bool
          rule n xs = elem n xs == task12 n xs

tests =
  TestList
    [ TestList [],
      task1Test,
      task2Test,
      TestList [],
      task4Test,
      task5Test,
      task6Test,
      task7Test,
      task8Test,
      task9Test,
      task10Test,
      task11Test,
      task12Test
    ]

main = runTestTT tests
