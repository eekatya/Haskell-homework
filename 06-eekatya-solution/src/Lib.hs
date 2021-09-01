{-# LANGUAGE InstanceSigs              #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE RankNTypes                #-}

module Lib
  ( Tree(..)
  , Church(..)
  , MyNum(..)
  , MyEnum(..)
  , ReversedList(..)
  , PairReaderList(..)
  , SortableIntegral(..)
  , task1
  , task4
  , task5
  , task6
  , task7
  , task8
  , task9
  , task10
  )
where

{- Разминка в интерпретаторе.
* :i Functor
* :doc Functor
* map (^2) [1..10]
* fmap (^2) [1..10]
* fmap show (3, 4)
* fmap (^2) (3, 4)
* fmap show (\x -> x * 2) 4
* fmap show $ Just 8
* (^2) <$> [1..10]
* :i (+)
-}

{- 1. Дана структура данных, описывающая непустое упорядоченное корневое
дерево. Напишите экземпляр Functor для этого дерева. -}
data Tree a = Node
  { value    :: a,
    children :: [Tree a]
  } deriving (Show, Eq)



instance Functor Tree where
  fmap :: (a -> b) -> Tree a -> Tree b
  fmap f (Node value children) = Node (f value) (map (fmap f) children)



task1 :: (a -> b) -> Tree a -> Tree b
task1 = fmap

{- 2. Улучшим аналогию классов типов со структурами данных. Определите структуру
данных MyFunctor такую, чтобы работал следующий код, если его раскомментировать:
-}


newtype MyFunctor a b f = MkMyFunctor
   {
        myFmap ::  forall a b . (a -> b) -> f a -> f b
   }


myListFmap :: MyFunctor a b []
myListFmap = MkMyFunctor map


myMaybeFmap :: MyFunctor a b Maybe
myMaybeFmap = MkMyFunctor $ \f m -> case m of
  Nothing -> Nothing
  Just a  -> Just (f a)

t1 = myFmap myListFmap (^ 2) [1 .. 10]

t2 = myFmap myListFmap show [1 .. 10]

t3 = myFmap myMaybeFmap ('y' :) (Just "x")

{- 3. Улучшите задание 2, чтобы дополнительно работал следующий код после
раскомментирования: -}

t4 = go myListFmap
  where go d = const (myFmap d (^ 2) [1 .. 10]) (myFmap d ('y' :) ["a"])

{- Для начала поймите, почему возникает ошибка про то, что Char не Num,
внимательно изучив вывод компилятора. Затем придумайте, как воспользоваться
forall из расширения RankNTypes. -}

{- 4. Дана обёртка над списками. Реализуйте для неё Show так, чтобы
оно работало как для нормальных списков, но внутри хранило список в
развёрнутом виде.

Например,
show (ReversedList [1,2,3]) == "[3,2,1]"

При решении этого задания нельзя пользоваться вызывать экземпляр Show для
списков, однако можно найти исходники этого экземпляра и скопировать оттуда
нужные куски кода. Если будете так делать, то сопроводите код несколькими
комментариями (может, с примерами исполнения), описывающими, как всё
работает. -}

newtype ReversedList a = ReversedList {getReversedList :: [a]}

instance Show a => Show (ReversedList a) where
  show :: ReversedList a -> String
  show (ReversedList a) = "[" ++ pr a ++ "]"
   where
    pr :: (Show a) => [a] -> String
    pr []       = ""
    pr [x     ] = show x
    pr (x : xs) = pr xs ++ "," ++ show x


task4 :: (Show a) => ReversedList a -> String
task4 = show

{- 5. Даны числа Чёрча. Сделайте их экземпляром Enum.

Если будете решать следующее задание, то `toEnum` легко определить так:
toEnum = fromInteger . toInteger -}
newtype Church = Church (forall a. (a -> a) -> a -> a)
-- Чтобы тесты компилировались ещё до реализации Enum Church.
data MyEnum a = MyEnum
  { mySucc     :: a -> a,
    myPred     :: a -> a,
    myToEnum   :: Int -> a,
    myFromEnum :: a -> Int
  }


instance Enum Church where
  succ :: Church -> Church
  succ (Church f) = Church $ \s z -> f s (s z)
  toEnum :: Int -> Church
  toEnum = fromInteger . toInteger
  fromEnum :: Church -> Int
  fromEnum (Church f) = f (+ 1) 0
  pred :: Church -> Church
  pred a = toEnum $ fromEnum a - 1


task5 :: MyEnum Church
task5 = MyEnum succ pred toEnum fromEnum

{- 6. Сделайте числа Чёрча экземпляром Num.
Подсказка: сложение может выглядеть вот так:
(Church a) + (Church b) = Church (\s z -> a s (b s z))
Учтите, что некоторые знакомые определения не протипизируются.
-}

-- Чтобы тесты компилировались ещё до реализации Num Church.
data MyNum a = MyNum
  { myPlus        :: a -> a -> a,
    myMult        :: a -> a -> a,
    myMinus       :: a -> a -> a,
    myAbs         :: a -> a,
    mySignum      :: a -> a,
    myFromInteger :: Integer -> a
  }

instance Num Church where
  (Church a) + (Church b) = Church $ \s z -> a s $ b s z
  (Church a) * (Church b) = Church $ \s z -> a (b s) z
  (Church a) - (Church b) = toEnum $ fromEnum (Church a) - fromEnum (Church b)
  abs (Church a) = Church a
  signum (Church a) | fromEnum (Church a) == 0 = 0
                    | otherwise                = 1

  fromInteger n
    | n <= 0
    = Church $ \f -> \x -> x
    | otherwise
    = let (Church p) = fromInteger (n - 1) in Church $ \f -> \x -> p f (f x)
            --      | otherwise = Church $ \f -> \x -> f ((unChurch $ fromInteger (n-1)) f x)

task6 :: MyNum Church
task6 = MyNum (+) (*) (-) abs signum fromInteger

{- 7. Сделайте данный тип представителем функтора. Попробуйте для этого
воспользоваться тем фактом, что список, функциональная стрелка и пара уже
являются функторами. -}

newtype PairReaderList e p a = PairReaderList
  { runPairReaderList :: [(e -> (p, a))]
  }

task7 :: (a -> b) -> PairReaderList e p a -> PairReaderList e p b
task7 = undefined -- замените undefined на fmap, когда будет готов экземпляр.

{- 8. Даны две функции. Пользуясь тем, что Maybe, пара и список являются
представителями Functor, реализуйте такой терм task8, что две предоставленные
функции корректно протипизируются. Пользуясь сервисом pointfree.io, запишите
task8 в бесточечном стиле. Напишите тип терма task8. -}

functionA :: (a -> b) -> Maybe (e, [Maybe a]) -> Maybe (e, [Maybe b])
-- Нельзя менять ни тип, ни реализацию!
functionA = task8

functionB :: (a -> b) -> (e, [[Maybe a]]) -> (e, [[Maybe b]])
-- Нельзя менять ни тип, ни реализацию!
functionB = task8

task8 = fmap . fmap . fmap . fmap

{- Определите класс типов Sortable, в котором есть одна операция
sort :: a -> a. Реализуйте двух представителей этого класса типов:
9.  Списки элементов, которые являются представителями Ord, сортируются с
    помощью Data.List.sort. Чтобы не было конфликта имён, осуществите
    квалифицированный импорт.
10. SortableIntegral n для любого представителя Integral n.
    Экземпляр должен сортировать цифры в двоичном представлении числа,
    сохраняя знак. Например, sort (-1042) == (-124).
-}

newtype SortableIntegral n = MkSortableIntegral {runSortableIntegral :: n}

task9 :: Ord a => [a] -> [a]
task9 = undefined -- замените undefined на sort, когда сделаете экземпляр.

task10 :: Integral n => SortableIntegral n -> SortableIntegral n
task10 = undefined -- замените undefined на sort, когда сделаете экземпляр.
