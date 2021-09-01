{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE InstanceSigs              #-}

{- РАЗМИНКА.

1. Если известно, что двойное буррито --- это буррито и что
   из начинки буррито X можно сделать бурито Y, то из буррито X можно
   сделать буррито Y. Действительно: заменим начинку бурито X целиком
   на бурито Y; получим двойное бурито Y. Но двойное бурито --- это
   бурито.

   Выразите (>>=) через join и (<$>).

2. Если известно, что можно получить буррито Y из буррито X, умея
   доготавливать начинку буррито X до полноценного буррито, то всякое двойное
   буррито является буррито. Действительно: возьмём какое-то двойное буррито Z и
   доготовим его начинку (обычное буррито Z) до обычного буррито Z очень простым
   образом: просто не будем ничего делать с ним.

   Выразите join через (>>=).

3. Убедитесь, что хотя автору концепции монад как буррито его аналогии
   могут показаться очевидными, стороннему наблюдателю сложно проследить за
   ними.

4. Выразите fmap через (>>=) и return.

5. (<*>) через (>>=) и return.

6. fmap через (<*>) и pure.

7. Напишите функцию, которая принимает список чисел и возвращает его же,
   где каждое чётное число повторяется дважды, используя монаду списка.
   [1,2,3,4] |-> [1,2,2,3,4,4].

8. n-гранный кубик кинули трижды. Известно, что сумма всех выпавших значений ---
   число чётное. Найдите вероятность того, что выпало число 14, используя монаду
   списков.

-}

module Lib
  ( FilterList(..)
  , filterListToList
  , Point(..)
  , task3
  , task4
  , task5
  , task6
  , task7
  )
where

import           Control.Monad                  ( replicateM
                                                , ap
                                                , guard
                                                )
import           Data.Maybe                     ( fromMaybe )
import           GHC.Generics                   ( Generic )
import           Data.Semigroup                 ( Sum(..) )

{- 1. Дана структура данных `FilterList`, которая содержит в себе пару из
некоторого предиката над `e` и значения `a`. Будем говорить, что значение `a`
активируется предикатом `p`, если лежит с ним в одной паре.

Пользоваться этой структурой данных предлагается через функцию
`filterListToList`, которая принимает на вход значение-активатор `e` и
возвращает только те `a`, активируемые предикатом, на котором значение-активатор
возвращает `True`. Реализация дана.

Сделайте `FilterList e` монадой с такой семантикой, что
`join :: FilterList e (FilterList e a) -> FilterList e a` составляет
экземпляр `FilterList e a`, в котором находятся все значения вложенных
`FilterList e a`, активируемые при условии, что активировался и предикат во
вложенном, и предикат во внешнем `FilterList e`. Порядок в новом списке на
усмотрение реализующего.

Например,
    sort $
    filterListToList 4 $
    join $
      FilterList [
        ((> 2),
          FilterList [
            ((< 5), 'x'),
            ((< 3), 'y'),
            ((< 2), 'z')
          ]
        ),
        (odd,
          FilterList [
            (\e -> e `mod` 3 == 0, 'a'),
            ((== 4), 'b'),
            ((== 5), 'c')
          ]
        ),
        (\e -> abs e <= 100,
          FilterList [
            (\e -> e `mod` 3 == 0, 'm'),
            ((== 4), 'n'),
            ((\e -> abs e <= 10), 'n')
          ]
        )
      ]
    ==
    "nnx"

Ни 'a', ни 'b', ни 'c' не подходят, потому что 4 не проходит внешнее условие
`odd e`; 'y' и 'z' не проходят внутренние условия `(< 3)` и `(< 2)`, а `m`
не проходит внутреннее условие, хотя и проходит внешнее.

Семантику `return` надо выбрать исходя из соображений о выполнении законов
монад и аппликативных функторов. При проверке законов можно считать, что два
`FilterList` равны, если на любом `e` результат вызова `filterListToList` с ними
будет отличаться только порядком элементов.
-}
newtype FilterList e a = FilterList [(e -> Bool, a)] deriving (Generic, Functor)

filterListToList :: e -> FilterList e a -> [a]
filterListToList e (FilterList xs) = snd <$> filter (\(p, a) -> p e) xs

instance Applicative (FilterList e) where
  pure a = undefined
  (<*>) = undefined

instance Monad (FilterList e) where
  return = undefined
  (FilterList ma) >>= f = undefined

{- 2. Дана структура данных 'Point', которая задаёт координату на плоскости.
Сделайте её монадой с такой семантикой, что вычисление проводится параллельно
над обеими компонентами. Например,

    do a <- Point 3 4
       b <- Point 10 12
       pure (a + b)
    == Point 13 16
-}

data Point a = Point a a deriving (Show, Eq, Functor, Generic)

instance Applicative Point where
  pure :: a -> Point a
  pure = return
  (<*>) :: Point (a -> b) -> Point a -> Point b
  (<*>) = ap

instance Monad Point where
  return :: a -> Point a
  return a = Point a a
  (>>=) :: Point a -> (a -> Point b) -> Point b
  Point a b >>= f = Point a' b'   where
    Point a' _  = f a
    Point _  b' = f b


{- 3. Дан терм. Перепишите его без do-нотации. -}

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
  [x, y]    <- b
  [x, y, z] <- a
  x         <- x
  x
  pure x

task3 a b =
  a
    >>  b
    >>  (if 3 < 5 then a >> return 4 else return 5)
    >>  a
    >>= (\x ->
          let y = 5
          in  b >>= \el -> case el of
                [x, y] -> a >>= \el -> case el of
                  [x, y, z] -> x >>= \x -> x >> pure x
                  _         -> fail ""
                _ -> fail ""
        )

{- 4. Напишите функцию, которая генерирует список троек чисел, в которых первая
компонента больше второй, а вторая больше третьей, а сами числа являются
сторонами прямоугольного треугольника. Порядок на этих тройках должен быть
лексикографический. Функция должна использовать монаду списка.

Первые пять элементов списка, который должен генерироваться:
[(5,4,3),(10,8,6),(13,12,5),(15,12,9),(17,15,8)]. -}

task4 = do
  z <- [1 ..]
  x <- [1 .. z]
  y <- [1 .. x]
  guard (x ^ 2 + y ^ 2 == z ^ 2)
  return (z, x, y)

{- 5. Напишите ту же функцию, что в предыдущем задании, но через list
comprehensions. -}

task5 =
  [ (z, x, y)
  | z <- [1 ..]
  , x <- [1 .. z]
  , y <- [1 .. x]
  , x ^ 2 + y ^ 2 == z ^ 2
  ]

{- 6. Напишите, используя монаду списков и функцию replicate, такую функцию,
которая принимает на вход список чисел и возвращает список, в котором каждое
число n заменено на n вхождений числа n. Например, список [1, 2, 3, 4]
должен превратиться в [1, 2, 2, 3, 3, 3, 4, 4, 4, 4]. -}

task6 xs = do
  x <- xs
  y <- replicate x x
  return y

{- 7. Акула захотела поиграть в покер, однако, будучи малоразумным животным,
не может ни у кого спросить, какие у этой игры правила. Более того, так как
акула --- животное морское, в карты играть не выйдет: они размокнут, да и само
предположение, что акула тасовала бы карты, звучит абсурдно.

В итоге акула играет в покер по таким правилам: сначала она кидает k-гранные
кости (игральные) один раз и получает число n от 1 до k. Затем акула кидает
кости ещё n раз и получает числа m_1, m_2, ..., m_n. Наконец, акула
подсчитывает сумму всех выпавших ей значений (n, m_1, ..., m_n), и если
число оказывается простым, то она считает себя победителем и может полакомиться
проигравшим.

С какой вероятностью акула победит? Определите, используя монаду списков. -}

-- |Функция, которая перебирает все возможные варианты развития событий
-- и, если сумма значений оказалась простым числом, возвращает пару из
-- вероятности такого варианта и сам вариант в виде списка всех полученных
-- значений, а если нет, то игнорирует данный вариант.
--
-- Например, для k = 2 результат был бы такой, если бы мы не выкидывали
-- все составные числа:
-- [(Sum {getSum = 0.250},[1,1]),
--  (Sum {getSum = 0.250},[1,2]),
--  (Sum {getSum = 0.125},[2,1,1]),
--  (Sum {getSum = 0.125},[2,1,2]),
--  (Sum {getSum = 0.125},[2,2,1]),
--  (Sum {getSum = 0.125},[2,2,2])]
--
-- Так как мы оставляем лишь простые числа, результат будет таким:
-- [(Sum {getSum = 0.250},[1,1]),
--  (Sum {getSum = 0.250},[1,2]),
--  (Sum {getSum = 0.125},[2,1,2]),
--  (Sum {getSum = 0.125},[2,2,1])]
allOutcomes :: Int -> [(Sum Double, [Int])]
allOutcomes k = undefined

-- Проверяется только эта функция, выше вспомогательная, которую можно
-- игнорировать, если есть идеи получше.
task7 :: Int -> Double
task7 k = undefined

{- РЕШЕНИЯ ЗАДАЧ ИЗ РАЗМИНКИ.

1. bind ma f = join (f <$> ma)

2. join mma = mma >>= id

3. Прямое следствие из предыдущих двух пунктов.

4. fmap' f ma = do v <- ma
                   return (f ma)
   fmap'' f ma = ma >>= (return . f)

5. ap mf ma = do f <- mf
                 a <- ma
                 return (f a)

   ap' mf ma = mf >>= (\f -> f <$> ma)

   Такая функция уже есть в стандартной библиотеке. Ей стоит пользоваться для
   того, чтобы просто реализовывать монады, не беспокоясь об аппликативных
   функторах.

6. Есть даже такой закон:
   fmap'' f fa = pure f <*> fa

7. f xs = do { x <- xs; if even x then [x, x] else [x] }

8. Например, так:

import Data.Function (on)

probability :: (a -> Bool) -- ^ Какие исходы желаемые
            -> [a]         -- ^ Все возможные исходы
            -> Double      -- ^ Вероятность желаемого исхода
probability p outcomes = ((/) `on` (fromIntegral . length))
                         (filter p outcomes)
                         outcomes

prob :: Int -> Double
prob n = probability (==14) allEvenOutcomes
    where toss = [1..n]
          allEvenOutcomes = do a <- toss
                               b <- toss
                               c <- toss
                               let s = a + b + c
                               True <- pure $ even s
                               pure s

Видно, что больше всего вероятность появления 14 при бросании десятигранного
кубика, что знать, безусловно, полезно.
-}
