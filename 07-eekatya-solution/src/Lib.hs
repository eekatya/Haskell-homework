{-# LANGUAGE DeriveFunctor             #-}
{-# LANGUAGE InstanceSigs              #-}
{-# LANGUAGE NamedFieldPuns            #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE RankNTypes                #-}

module Lib
  ( Gcd(..)
  , Tree(..)
  , Heap(..)
  , task1
  , task2
  , task4
  , task5
  , task6
  , task7
  , task8
  , task9
  , task10
  , task11
  , task12
  )
where

import           Data.List                      ( foldl'
                                                , unfoldr
                                                )
import           Data.Monoid                    ( Sum(Sum)
                                                , getSum
                                                )

{- 1. Задаёт ли операция поиска наибольшего общего делителя моноид? Полугруппу?
Если да, реализуйте соответствующие экземпляры для обёртки `Gcd`. Если нет,
предоставьте контрпример. Контрпример для полугруппы -- тройка элементов, на
которых нарушается ассоциативность (то есть `a <> (b <> c) != (a <> b) <> c`),
а для моноида -- такой элемент `a`, что не существует такого элемента `e`, что
`e <> a == a` и `a <> e == a`. -}

newtype Gcd a = Gcd a deriving (Eq, Show)

instance (Integral a) => Semigroup (Gcd a) where
  (<>) (Gcd l) (Gcd r) = Gcd $ gcd l r

{- Left (a, b, c), если gcd a (gcd b c) /= gcd (gcd a b) c для каких-то a, b и c.
В противном случае Right (<>, ???), где ??? -- это
Right mempty, если определён моноид; Left a, если для какого-то `a` нет левой
или правой единицы. -}
task1
  :: Integral a => Either (a, a, a) (Gcd a -> Gcd a -> Gcd a, Either a (Gcd a))
task1 = Right ((<>), Left (-1))

{- 2. Задаёт ли моноид операция `\x y -> ((x * y) `div` x) * x` на числах типа
`Int`? А полугруппу? -}

newtype IntOp = IntOp Int deriving (Show, Eq)

task2 :: Either (Int, Int, Int) (IntOp -> IntOp -> IntOp, Either Int IntOp)
task2 = Left (2, 0, 5)

{- 3. Используя библиотеку QuickCheck, опишите правила, проверяющие предыдущее
задание. В качестве примера можно взять правила, определённые в тестовом модуле
для первой задачи. Не нужно разбирать тщательно task2, как это сделано в тестах
для task1: я-то не знаю, какой ответ даст студент, а вы уже заведомо в курсе,
какая часть тестов не будет участвовать в проверке.

Можно считать, что на `x == 0` тест заведомо проходит успешно, то есть
ограничиться случаями, где тесты не вылетают. -}

{- 4. Сделайте экземпляром `Foldable` дерево в представлении "левый ребёнок,
правый сосед":

    Node 1 ---------------- Node 2 ------------------------- Node 3 -- Leaf
   /                          /                               /
 Node 4 -- Leaf            Node 5 ------- Node 6 -- Leaf   Node 7 -- Leaf
   /                        /                /               /
 Leaf                    Node 8 -- Leaf   Node 9 -- Leaf   Leaf
                          /                /
                        Leaf             Node 10 -- Leaf
                                         /
                                       Leaf

Node 1 (Node 4 Leaf Leaf)
       (Node 2 (Node 5 (Node 8 Leaf Leaf)
                       (Node 6 (Node 9 (Node 10 Leaf Leaf) Leaf) Leaf))
               (Node 3 (Node 7 Leaf Leaf) Leaf))

Порядок обхода: сначала все вершины на первом уровне, потом все вершины на
втором и так далее. На представленном в примере дереве требуемый порядок обхода
совпадает с порядком на вершинах. -}

data Tree a
  = Leaf
  | Node
      { value         :: a,
        leftChild     :: Tree a,
        rightNeighbor :: Tree a
      }
  deriving (Show, Eq)

instance Foldable Tree where
  foldr :: (a -> b -> b) -> b -> Tree a -> b
  foldr = undefined

task4 :: Monoid m => (a -> m) -> Tree a -> m
task4 = undefined

{- 5. Напишите функцию, которая подсчитывает количество элементов,
соответствующих предикату, используя `foldMap` и моноид `Sum`. -}

countMatching :: (Foldable t, Num n) => (a -> Bool) -> t a -> n
countMatching p = getSum . foldMap (\x -> if p x then Sum 1 else Sum 0)

task5 = countMatching

{- 6. Несколько фактов:
* `uncurry` является свёрткой для пары.
* `either` является свёрткой для `Either`.
Значит, с помощью этих функций можно выразить что угодно, что можно было бы
сделать через сопоставление с образцом по данным типам.

Убедитесь в этом: напишите функцию, которая принимает на вход
`Either (Int, Int) String` и в случае, когда в экземпляре лежит пара, находит
сумму её элементов, а когда строка, то её длину. При этом нельзя делать
сопоставление с образцом или использовать функции помимо `foldr`, `uncurry`,
`either`, `+` и комбинаторов лямбда-исчисления (`const`, `(.)` и всего такого).
-}
task6 :: Either (Int, Int) String -> Int
task6 = either (uncurry (+)) (foldr (const (+ 1)) 0)

{- 7. Дана куча на минимум. Сделайте её экземпляром `Foldable` так, чтобы в
ветку исполнение не заходило до тех пор, пока этого можно избежать. -}

data Heap k v = HeapLeaf | HeapNode k v (Heap k v) (Heap k v)
  deriving (Show, Eq, Functor)

task7 :: (Ord k, Monoid m) => (v -> m) -> Heap k v -> m
task7 = undefined

{- 8. Используя `unfoldr`, реализуйте `map`, не используя конструкторы
списков. Решение должно работать на бесконечных списках. -}

destroy :: (forall a . (a -> Maybe (b, a)) -> a -> c) -> [b] -> c
destroy g xs = g listpsi xs
 where
  listpsi :: [a] -> Maybe (a, [a])
  listpsi []       = Nothing
  listpsi (x : xs) = Just (x, xs)

myMap :: (a -> b) -> [a] -> [b]
myMap f xs = destroy (\psi a -> unfoldr (mapDU psi) a) xs
 where
  mapDU psi xs = case psi xs of
    Nothing      -> Nothing
    Just (x, ys) -> Just (f x, ys)


task8 = myMap

{- 9. Используя `unfoldr`, реализуйте аналог `takeWhile p . iterate f`. -}

task9 :: (a -> Bool) -> (a -> a) -> a -> [a]
task9 f p = unfoldr (\x -> if f x then Just (x, p x) else Nothing)

{- 10. Без сопоставления с образцом и каких-либо сторонних функций, используя
один вызов `foldr`, реализуйте аналог `\xs n -> (take n xs, drop n xs)`.
Попробуйте для начала реализовать это обычной рекурсией в один проход, а затем,
как на слайдах практики с `reverse'`, произвести трансляцию в `foldr`. -}

--splitAt':: Int -> [a] -> ([a],[a])
--splitAt' 0 xs = ([], xs)
--splitAt' _ [] = ([],[])
--splitAt' n (x:xs) = (x:xs', y) where
--                    (xs', y) = splitAt' (n - 1) xs

task10 :: Int -> [a] -> ([a], [a])
task10 i xs = foldr f ini xs i
 where
  ini :: Int -> ([b], [b])
  ini _ = ([], [])
  f :: b -> (Int -> ([b], [b])) -> (Int -> ([b], [b]))
  f el g n =
    let func l r = if n <= 0 then ([], el : r) else (el : l, r)
    in  uncurry func $ g (n - 1)

{- 11. Реализуйте такую функцию, что к элементам списка на чётных позициях
применяется одна функция, а к элементам на нечётных -- другая.
Нужно поддержать бесконечные списки. Нельзя сопоставлять списки и пары с
образцом, но можно использовать функции `uncurry` и `foldr`.

Пример:
>>> take 10 $ alternatingMap (+ (-100)) (*3) [1..]
[-99,6,-97,12,-95,18,-93,24,-91,30]
-}

alternatingMap :: (a -> b) -> (a -> b) -> [a] -> [b]
alternatingMap = undefined

task11 = alternatingMap

{- 12. Используя `foldr`, напишите функцию, которая проверяет, есть ли заданный
элемент в списке. -}

myElem :: Eq a => a -> [a] -> Bool
myElem x = foldr (\y b -> y == x || b) False

task12 = myElem
