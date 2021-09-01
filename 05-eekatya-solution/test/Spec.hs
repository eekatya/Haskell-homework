import Control.Monad (forM)
import Data.Maybe (catMaybes, isNothing)
import Data.List (sort)
import Lib
import Test.HUnit
import Test.QuickCheck (chatty, isSuccess, output, quickCheckWithResult, stdArgs)

task1Test =
  TestList
    [ TestCase $ assertEqual "sumIntTree с одним числом, " 42 (task1 oneNum),
      TestCase $ assertEqual "sumIntTree без переполнения, " 15 (task1 noOverflow),
      TestCase $ assertEqual "sumIntTree с одним числом на каждом уровне, " (2 ^ 63 + 5) (task1 oneNumPerLevel),
      TestCase $ assertEqual "sumIntTree с переполнением на одном уровне, " (2 ^ 63 + 5) (task1 overflow)
    ]
  where
    oneNum = node 42 [leaf, leaf, leaf, leaf]
    noOverflow = node 1 [node 2 [node 5 [leaf]], leaf, node 3 [], leaf, node 4 []]
    oneNumPerLevel = node 5 [leaf, leaf, node (2 ^ 62) [node (2 ^ 62) [], leaf]]
    overflow = node 4 [node (2 ^ 62) [node 1 []], leaf, node (2 ^ 62) [], leaf]

task2Test =
  TestList
    [ TestCase $ assertEqual "foldNat (+1) 0 2, " 2 (task2 (+ 1) 0 (Suc (Suc Zero))),
      TestCase $ assertEqual "foldNat (+1) 0 1, " 1 (task2 (+ 1) 0 (Suc Zero)),
      TestCase $ assertEqual "foldNat (+1) 0 0, " 0 (task2 (+ 1) 0 Zero),
      TestCase $ assertEqual "2^3 через foldNat" 8 (task2 (* 2) 1 (Suc (Suc (Suc Zero)))),
      TestCase $ assertEqual "sssz через foldNat" "sssz" (task2 ('s' :) "z" (Suc (Suc (Suc Zero))))
    ]

task3Test =
  TestList $
    ( \(descr, test) ->
        TestCase $ do
          result <- test
          let message = descr ++ ", но\n" ++ output result
          if isSuccess result
            then pure ()
            else assertBool message False
    )
      <$> tests
  where
    qcArgs = stdArgs {chatty = False}
    tests =
      [ ( "Либо A < B, либо B < A, либо A == B",
          quickCheckWithResult qcArgs totality
        ),
        ( "Неправда, что A < A",
          quickCheckWithResult qcArgs irreflexivity
        ),
        ( "Если A < B, то неправда, что B < A",
          quickCheckWithResult qcArgs antisymmetry
        ),
        ( "Правило №1 из условия должно выполняться",
          quickCheckWithResult qcArgs rule1
        ),
        ( "Правило №2 из условия должно выполняться",
          quickCheckWithResult qcArgs rule2
        )
      ]
    totality :: String -> String -> Bool
    totality s1 s2 = s1 == s2 || task3 s1 s2 || task3 s2 s1
    irreflexivity :: String -> Bool
    irreflexivity s = not (task3 s s)
    antisymmetry :: String -> String -> Bool
    antisymmetry s1 s2 = s1 == s2 || not (task3 s1 s2 && task3 s2 s1)
    rule1 :: String -> Char -> String -> Bool
    rule1 s1 c s2 = task3 s1 (s1 ++ c : s2)
    rule2 :: String -> String -> String -> Bool
    rule2 s s1 s2 = task3 (s ++ s1) (s ++ s2) == task3 s1 s2

task4Test =
  TestList
    [ TestCase $ assertEqual "Just не меняет список, " (Just [1, 2, 3]) (task4 Just [1, 2, 3]),
      TestCase $ assertEqual "Nothing убирает список, " (Nothing :: Maybe [Bool]) (task4 (const Nothing) [1, 2, 3]),
      TestCase $ assertEqual "Nothing убирает *весь* список, " Nothing (task4 (\x -> if even x then Just x else Nothing) [1, 2, 3]),
      TestCase $ assertEqual "Список не убирается, если всё Just, " (Just [2, 4, 6]) (task4 (\x -> if even x then Just x else Nothing) [2, 4, 6]),
      TestCase $ assertEqual "Сложная функция, " (Just ["6", "10", "14"]) (task4 (Just . show . (* 2) . (+ 1)) [2, 4, 6]),
      TestCase $ assertEqual "Пустой список, " (Just []) (task4 (Just . show . (* 2) . (+ 1)) [])
    ]

task5Test =
  TestList
    [ TestCase $ assertEqual "Одно и то же число Фибоначчи несколько раз, " 4 (task5 [1, 1, 1, 1]),
      TestCase $ assertEqual "Много разных чисел Фибоначчи, " 5 (task5 [0, 1, 1, 2, 3]),
      TestCase $ assertEqual "Разные числа, " 3 (task5 [0, 1, 4, 6, 7, 8, 9, 11]),
      TestCase $ assertEqual "Пустой список, " 0 (task5 []),
      TestCase $ assertEqual "Ни одного числа Фибоначчи, " 0 (task5 [4, 9])
    ]

task6Test = TestCase $ assertBool "Тесты для №6 проходят, " (and task6Tests)

task7Test =
  TestList
    [ TestCase $ assertEqual "Единичное окно, " [[1], [2], [3]] ([1, 2, 3] `task7` 1),
      TestCase $ assertEqual "Окно размера 2, " [[1, 2], [2, 3]] ([1, 2, 3] `task7` 2),
      TestCase $ assertEqual "Окно, равное списку, " [[1, 2, 3]] ([1, 2, 3] `task7` 3),
      TestCase $ assertEqual "Окно размера 3, " [[1, 2, 3], [2, 3, 4], [3, 4, 5]] ([1, 2, 3, 4, 5] `task7` 3),
      TestCase $ assertEqual "Окно крупнее списка, " [] ([1] `task7` 2)
    ]

task8Test =
  TestList
    [ TestCase $ assertEqual "Just не меняет список, " [1, 2, 3] (task8 Just [1, 2, 3]),
      TestCase $ assertEqual "Nothing убирает список, " [] (task8 (const (Nothing :: Maybe Bool)) [1, 2, 3]),
      TestCase $ assertEqual "То, что Just, присутствует, " [2] (task8 (\x -> if even x then Just x else Nothing) [1, 2, 3]),
      TestCase $ assertEqual "Если всё Just, всё присутствует, " [2, 4, 6] (task8 (\x -> if even x then Just x else Nothing) [2, 4, 6]),
      TestCase $ assertEqual "Сложная функция, " ["6", "10", "14"] (task8 (Just . show . (* 2) . (+ 1)) [2, 4, 6]),
      TestCase $ assertEqual "Пустой список, " [] (task8 (Just . show . (* 2) . (+ 1)) [])
    ]

task9Test =
  TestList
    [ TestCase $ assertEqual "Пример №9 из условия, " 2 (task9 [1, -1, 3, -2, 3, 0]),
      TestCase $ assertEqual "Отсортированный список, " (100 -2) (task9 [1 .. 100]),
      TestCase $ assertEqual "Отсортированный в обратном порядке список, " 0 (task9 [100, 99 .. 0]),
      TestCase $ assertEqual "Пустой список, " 0 (task9 ([] :: [Int])),
      TestCase $ assertEqual "Список длины 1, " 0 (task9 [1])
    ]

task10Test =
  TestList
    [ TestCase $ assertEqual "Списки равной длины, " [3, 5 / 8, 3.5] (task10 [1, 2, 3, 4] [10, 10, 2, 3] [2, 4, 8, 2] 1),
      TestCase $ assertEqual "Списки длины X > Y > Z, " [3, 5 / 8, 3.5] (task10 [1, 2, 3, 4, 5, 6] [10, 10, 2, 3, 2, 2] [2, 4, 8, 2] 1),
      TestCase $ assertEqual "Списки длины Y > X > Z, " [3, 5 / 8, 3.5] (task10 [1, 2, 3, 4, 5, 6] [10, 10, 2, 3, 2, 2, 3, 1, 2] [2, 4, 8, 2] 1),
      TestCase $ assertEqual "Списки длины Z > X > Y, " [3, 5 / 8, 3.5] (task10 [1, 2, 3, 4, 5, 6] [10, 10, 2, 3] [2, 4, 8, 2, 3, 3, 3, 3] 1),
      TestCase $ assertEqual "Списки длины Z > Y > X, " [3, 5 / 8, 3.5] (task10 [1, 2, 3, 4] [10, 10, 2, 3, 5] [2, 4, 8, 2, 3, 3, 3, 3] 1),
      TestCase $ assertEqual "Позиция слишком большая, " [] (task10 [1, 2, 3, 4] [10, 10, 2, 3, 5] [2, 4, 8, 2, 3, 3, 3, 3] 1000),
      TestCase $ assertEqual "Позиция -- начало, " [5.5, 3, 5 / 8, 3.5] (task10 [1, 2, 3, 4] [10, 10, 2, 3, 5] [2, 4, 8, 2, 3, 3, 3, 3] 0)
    ]

task11Test =
  TestList
    [ TestCase $ assertEqual "Первый список пуст, " [] (task11 (*) [] [1 :: Int, 2, 3]),
      TestCase $ assertEqual "Второй список пуст, " [] (task11 (*) [1 :: Int, 2, 3] []),
      let f = \a b -> [a, b]
       in TestCase $
            assertEqual
              "Все элементы встречаются, "
              (sort (f <$> "abc" <*> "defgh"))
              (sort (task11 f "abc" "defgh")),
      let f = \a b -> [a, b]
       in TestCase $
            assertEqual
              "Все элементы встречаются (первый список длиннее), "
              (sort (f <$> "abcdef" <*> "xyz"))
              (sort (task11 f "abcdef" "xyz"))
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
      task10Test,
      task11Test
    ]

main = runTestTT tests
