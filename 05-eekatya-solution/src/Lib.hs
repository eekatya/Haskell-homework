{-# LANGUAGE NoMonomorphismRestriction #-}

module Lib
  ( IntTree(..)
  , -- Экспортировать со всеми конструкторами
    task1
  , node
  , leaf
  , Nat(..)
  , task2
  , task3
  , task4
  , task5
  , task6Tests
  , task7
  , task8
  , task9
  , task10
  , task11
  )
where

{- Разминка.

* let (x:y) = "GHC" in (x,y)
* let (x,y) = "GHC" in (x,y)
* let (x:y:z) = "GHC" in (x,y,z)
* let (x:y:z:t) = "GHC" in (x,y,z,t)
* let (x:y:z:t:r) = "GHC" in (x,y,z,t,r)
* let (x:[y,z]) = "GHC" in (x, y, z)
* let (x:y:[z]) = "GHC" in (x, y, z)
* let ([x,y]:[z]) = "GHC" in (x, y, z)
* let { x = tail y; y = 0 : 1 : zipWith (+) x y} in take 10 y
* :t []
* :t (:)
* :i []
* data B = B (B -> Int)
  -- Есть ли не использующий расходимость экземпляр B? Ответ внизу.
* data A = A (() -> A)
  -- Есть ли не использующий расходимость экземпляр A? Ответ внизу.
-}
import           Data.List                      ( genericDrop
                                                , genericLength
                                                , genericTake
                                                )
import           Data.Maybe                     ( catMaybes
                                                , fromJust
                                                , isJust
                                                , isNothing
                                                )

{- Задание 1.
Реализуйте тип данных IntTree, который может быть построен одним из двух
способов:
* Leaf: В экземпляре нет дополнительных данных;
* Node: В экземпляре содержится число типа Int (не Integer!), а также список
  IntTree.
Напишите функцию sumIntTree :: IntTree -> Integer, которая просуммирует все
значения в дереве. -}
data IntTree = Leaf | Node Int [IntTree]

{- Когда определите структуру данных, замените эти две строки на

node = Node
leaf = Leaf

Это изощрение нужно, чтобы тестовый файл компилировался ещё до появления
конструкторов IntTree. -}
node = Node
leaf = Leaf

sumIntTree :: IntTree -> Integer
sumIntTree Leaf       = 0
sumIntTree (Node x a) = toInteger x + sum (map sumIntTree a)

task1 = sumIntTree

{- Задание 2.
Реализуйте функцию foldNat (Nat такой же, как на лекции) с поведением
`foldNat (+1) 0 (Suc (Suc (Suc Zero))) == 3`. -}
data Nat = Zero | Suc Nat

foldNat :: (a -> a) -> a -> Nat -> a
foldNat f x Zero    = x
foldNat f x (Suc n) = foldNat f (f x) n

task2 = foldNat

{- Задание 3.
Реализуйте функцию, которая сравнивает два списка лексикографически:
1. Если список A является префиксом B и не равен B, то A < B;
2. Если есть такой индекс i, что i-ый элемент A отличается от i-ого элемента
  B, а все предыдущие элементы равны, то меньше тот список, у которого i-ый
  элемент меньше.
Для сравнения можно использовать операторы (<), (<=), (>), (>=), (==), (/=).
-}
lexLess :: (Ord a) => [a] -> [a] -> Bool
lexLess _  [] = False
lexLess [] _  = True
lexLess (x : xs) (y : ys) | x < y  = True
                          | x > y  = False
                          | x == y = lexLess xs ys

task3 = lexLess

{- Задание 4.
Реализуйте функцию, которая будет применять к каждому элементу списка
заданную функцию, а затем, если все элементы являются Just, "снимать"
Just и возвращать список элементов, а если есть хотя бы один Nothing,
то возвращать Nothing. -}
applyMaybeList :: (a -> Maybe b) -> [a] -> Maybe [b]
applyMaybeList f el | any isNothing (map f el) = Nothing
                    | otherwise                = Just (catMaybes (map f el))


task4 = applyMaybeList

{- Задание 5.
Реализуйте функцию, которая подсчитывает количество элементов списка,
которые являются числами Фибоначчи. -}
belongToFib :: Integer -> Bool
belongToFib n = helper 0 1
 where
  helper curr prev | curr == n = True
                   | curr > n  = False
                   | curr < n  = helper (curr + prev) curr
countFibs :: [Integer] -> Integer
countFibs [] = 0
countFibs (x : xs) | belongToFib x = countFibs xs + 1
                   | otherwise     = countFibs xs

task5 = countFibs

{- Задание 6.
Задайте структуру данных, которая определяет лямбда-термы, в которых
переменные идентифицируются строками. Напишите функцию, которая
проверяет, является ли некоторая строка идентификатором свободной
переменной в данном терме. Напишите несколько тестов для этой функции.

Равенство строк можно проверять с помощью оператора (==).

Примеры тестов (с псевдокодом):
* Определяется ли переменная как свободная, если она вообще не
  встречается в терме
  not (z `isFreeIn` (\x y -> x));
* Определяется ли переменная как свободная, если она на самом деле
  связанная
  not (x `isFreeIn` (\x -> ...));
* Определяется ли в терме-переменной его переменная как свободная
  (x `isFreeIn` x);
* Правда ли, что в терме-переменной любая другая переменная не определяется
  как свободная
  not (y `isFreeIn` x);
* Правда ли, что в аппликации, слева в которой переменная свободная, а
  справа нет, она будет определена как свободная
  (x `isFreeIn` x (\y -> y));
* Симметрично
  (x `isFreeIn` (\y -> y) x).

Дополнительный балл можно получить, если оформить эти тесты в QuickCheck. -}

{- Если хотите, можете сделать более правильный набор тестов в более правильном
файле. Тут представлен самый простой вариант: список выражений, все из которых
должны быть True, чтобы тест прошёл. -}
task6Tests :: [Bool]
task6Tests = []

{- Задание 7.
Реализуйте функцию, которая проходит по списку с "окном" заданного размера:
[1..6] `withWindow` 3 == [[1, 2, 3], [2, 3, 4], [3, 4, 5], [4, 5, 6]].
Длина каждого списка в результирующем списке должна быть равна окну.
Поведение с окном неположительного размера не проверяется. -}
withWindow :: [a] -> Integer -> [[a]]
withWindow xs n | n <= 0 = error "Invalid window size"
withWindow xs n | genericLength xs < n = []
withWindow xs n = genericTake n xs : withWindow (tail xs) n

task7 = withWindow

{- Задание 8.
Реализуйте функцию, которая будет применять к каждому элементу списка
заданную функцию, а затем возвращает список всего, что лежит внутри Just. -}
applyMaybeList' :: (a -> Maybe b) -> [a] -> [b]
applyMaybeList' f x = map fromJust (filter isJust (map f x))

task8 = applyMaybeList'

{- Задания 9-11.
Пересмотрите слайды лекции и найдите определения функций zipWith, drop, take,
map, filter, length. Далее с помощью системы Hoogle найдите функцию с типом
[[a]] -> [a]. Используя её и перечисленные функции, *без явного сопоставления с
образцом и других функций, которые оперируют списками (например, нельзя
делать list comprehensions, так как они внутри используют оператор (>>=))*,
задайте следующие функции: -}

-- См. общий комментарий к заданиям 9-11!
-- |Количество элементов в списке, которые больше, чем элемент на 2 раньше.
-- Например, в списке [1, -1, 3, -2, 3, 0] таких элементов два: это
-- 0 и первое вхождение 3.
-- Посмотрите на fibs из лекции для вдохновения.
-- "Ord a" означает, что элементы можно сравнивать между собой.
task9 :: Ord a => [a] -> Int
task9 = undefined

-- См. общий комментарий к заданиям 9-11!
-- |Data-Oriented Design.
-- Лютая распараллеливающая система отдельно считает список чисел X, отдельно --
-- список Y, отдельно -- список Z. В конечном счёте нужно найти список
-- (x1 + y1) / z1, (x2 + y2) / z2, (x3 + y3) / z3...
-- Проблема в том, что списки X, Y и Z могли генерироваться с разной скоростью.
--
-- Задача: по текущим показаниям с X, Y и Z получить как можно больше
-- (X + Y) / Z, начиная с некой позиции i (например, если первые i результатов
-- мы уже сообщали ранее и они уже никому не нужны).
--
-- Деление на 0 можно не обрабатывать особо.
task10
  :: [Double] -- ^Список X
  -> [Double] -- ^Список Y
  -> [Double] -- ^Список Z
  -> Int   -- ^Начальная позиция
  -> [Double] -- ^(X + Y) / Z
task10 = undefined

-- См. общий комментарий к заданиям 9-11!
-- |Функция, которая принимает на вход функцию и два списка и возвращает все
-- результаты применения функции к каждой паре элементов этих списков.
task11 :: (a -> b -> c) -> [a] -> [b] -> [c]
task11 _ []       _        = []
task11 _ _        []       = []
task11 f (x : xs) (y : ys) = f x y : task11 f xs ys

{- Ответы. --------------------------------------------------------------------
B (\x -> 3), да и вообще вместо 3 что угодно. На самом деле, экземпляров очень
много.

Не использующий расходимость A есть, но только в таком духе:
a :: A
a = A (\() -> a)
Это пример бесконечного экземпляра структуры данных. -}
