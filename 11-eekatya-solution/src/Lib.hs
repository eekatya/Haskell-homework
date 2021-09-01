{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module Lib
  ( StateReader
  , runStateReader
  , stateAsk
  , stateLocal
  , stateReader
  , stateAsks
  , StateWriter
  , runStateWriter
  , stateWriter
  , stateTell
  , stateListen
  , stateListens
  , stateCensor
  , whileM_
  , task3
  , task4
  , decrementIORef
  , task6
  , MonadRandom(..)
  , runMonadRandom
  , reinit
  , nextInt
  , nextValue
  , task8
  , Lam(..)
  , task9
  , task10
  )
where

{-
Разминка:

0. В README есть примеры ленивых вычислений.

1. Прочитайте строчка за строчкой файл Control.SimpleMonad. Если какая-то
   монада непонятна, сотрите её определение и попробуйте написать своё. По
   опыту, это единственный действенный способ научиться монадам.

2. Вбейте в интерпретатор:
   :help run

   putStrLn "x"
   :run putStrLn "x" -- упадёт
   x = putStrLn "x"
   :run x

   import System.Environment
   :run getArgs
   :run getArgs 3
   :run getArgs ["x", "y", "z"]

3. Дана программа:

myMap :: (a -> b) -> [a] -> [b]
myMap f xs = go xs []
  where go [] xs' = reverse xs'
        go (x:xs) xs' = go xs $! (f x:xs')

С одной стороны, она делает два прохода по списку -- один в go, другой в
reverse -- и тем самым вроде как должна быть менее производительной. С другой,
здесь используется строго форсируемый аккумулятор, который должен ускорить
вычисления. Лучше или хуже эта версия, чем классический map? (Ответ "раз map
определён так, как он определён, то он лучше" не принимается, поскольку Haskell
задумывается как язык и для изучения ФП, из-за чего многие определения в
стандартной библиотеке держат более читаемыми даже ценой других характеристик
вроде производительности.

Ответ: myMap совершенно во всём хуже, чем map. Форсирование тут бесполезно хотя
бы потому, что доводит до WHNF только сам список `(f x:xs')`, который и так уже
в WHNF, так как является вызовом `(:)`. Аккумулятор приводит к тому, что до тех
пор, пока мы не пройдёмся по всему списку, результат myMap будет накапливаться
в аккумуляторе, занимая O(длина списка) памяти (что подразумевает, что, в
отличие от map, на бесконечных списках myMap не сработает).

В Хаскелле есть только один способ что-то сделать со структурой данных, и это
сопоставление с образцом. Значит, кто бы ни вызывал myMap, он это делает с
рассчётом тут же сопоставить результат вызова с образцом: либо это, либо
результат выкидывается, но тогда бы myMap не стал и вычисляться. Таким образом,
в функциональном программировании лучшее мерило производительности функции --
это количество редукций, необходимых до выдачи чего-то, на чём можно
сопоставиться с образцом. По этой метрике map выигрывает у myMap до смешного:
    > map f [1, 2, 3] -> f 1 : map f [2, 3]
в одну редукцию. Раз map вычислился хотя бы до этого, значит, кто-то запросил
доведение его как минимум до WHNF, а эту задачу удалось выполнить очень быстро.
    > myMap f [1, 2, 3] = go [1, 2, 3] []
      -> go [2, 3] [f 1]
      -> go [3] [f 2, f 1]
      -> go [] [f 3, f 2, f 1] = reverse [f 3, f 2, f 1]
и ещё какое-то количество редукций на получение `f 1` из развёрнутого списка.
Итого, у myMap ноль преимуществ перед map и груда недостатков.

4. Даны две программы:
length1 [] = 0
length1 (x:xs) = 1 + length xs

length2 xs = go xs 0
  where go [] n = n
        go (x:xs) n = go xs $! 1 + n

Какая из них лучше?

Ответ: посмотрим в исходники:
https://hackage.haskell.org/package/base-4.14.0.0/docs/src/Data.OldList.html#genericLength

    genericLength []        =  0
    genericLength (_:l)     =  1 + genericLength l

    {-# RULES
      "genericLengthInt"     genericLength = (strictGenericLength :: [a] -> Int);
      "genericLengthInteger" genericLength = (strictGenericLength :: [a] -> Integer);
     #-}

    strictGenericLength     :: (Num i) => [b] -> i
    strictGenericLength l   =  gl l 0
                            where
                               gl [] a     = a
                               gl (_:xs) a = let a' = a + 1 in a' `seq` gl xs a'

Итак, здесь сказано, что в общем случае используется length1, но для Int и
Integer -- как раз вторая версия. Почему так?

Дело в том, что мы не можем заранее знать, какая именно реализация Num будет
использована для подсчёта длины, а без этого невозможно предсказать, какая
реализация оптимальнее.

Например, возьмём
data Nat = Zero | Suc Nat

Если реализовать сложение для таких чисел неким конкретным образом, то
`length1 [1..] :: Nat` будет тут же выдавать бесконечное натуральное число вида
`Suc (Suc (Suc ...`, которое можно тут же сопоставить с образцом. Например, так
можно выполнить код `(length1 [1..] :: Nat) > 35` и убедиться, что у
бесконечного списка длина больше 35. Польза от этого спорная, но вызов
`length2 [1..] :: Nat` вообще зависнет и будет потреблять всё больше памяти.

В противовес случаю с Nat, если посмотреть на Int, то length2 сильно лучше.
Дело в том, что `(1 +)` для `Int` обязательно строгий в своём аргументе и не
может вернуть частичный результат. Следовательно, чтобы как можно быстрее
вернуть хоть WHNF, если это вообще возможно, в случае с `Int` или `Integer`
необходимо полностью вычислить весь ответ на запрос длины списка, получить
информацию о том, что накопленное число уже больше 35, и прекратить рассчёты
заранее невозможно.

На бесконечном списке `length1` для `Int` будет накапливать цепочку ящиков вида
`1 + (1 + (1 + (1 + ...`, а `length2` зависнет, но будет использовать
константную память. -}

import           Control.Arrow                  ( Arrow((***))
                                                , (&&&)
                                                )
import           Control.Monad                  ( replicateM
                                                , replicateM_
                                                )
import           Control.SimpleMonad
import           Data.Bool                      ( bool )
import           Data.Function                  ( fix )
import           Data.Functor                   ( (<&>) )
import           Data.Functor.Identity
import           Data.IORef
import           Data.Semigroup                 ( Endo(..) )
import qualified Data.Set                      as S
import           GHC.Generics                   ( Generic )
import           System.Environment             ( getArgs )
import           System.Random

{- 1. С помощью монады State можно реализовать всё, что умеет монада Reader.
Реализуйте представленные функции, чтобы в этом убедиться. -}

-- Смените на любое удобное определение, включающее State, но не Reader.
type StateReader e a = State e a

runStateReader :: StateReader e a -> e -> a
runStateReader = evalState

stateAsk :: StateReader e e
stateAsk = stateAsks id

stateLocal :: (e -> e) -> StateReader e a -> StateReader e a
stateLocal f m = stateReader $ \e -> runStateReader m (f e)

stateReader :: (e -> a) -> StateReader e a
stateReader f = state (\e -> (f e, e))

stateAsks :: (e -> a) -> StateReader e a
stateAsks = stateReader

{- 2. С помощью монады State можно реализовать всё, что умеет монада Writer.
Реализуйте представленные функции, чтобы в этом убедиться. Обратите внимание
на то, что моноиды не обязательно являются коммутативными. -}

-- Смените на любое удобное определение, включающее State, но не Writer.
type StateWriter w a = State w a

runStateWriter :: Monoid w => StateWriter w a -> (a, w)
runStateWriter m = runState m mempty

stateWriter :: Monoid w => (a, w) -> StateWriter w a
stateWriter (a, w) = state (\e -> (a, e <> w))

stateTell :: Monoid w => w -> StateWriter w ()
stateTell w = state (\e -> ((), e <> w))

stateListen :: Monoid w => StateWriter w a -> StateWriter w (a, w)
stateListen m =
  state (\e -> let (a, w) = runStateWriter m in ((a, w), e <> w))

stateListens :: Monoid w => (w -> b) -> StateWriter w a -> StateWriter w (a, b)
stateListens f m =
  state (\e -> let (a, w) = runStateWriter m in ((a, f w), e <> w))



stateCensor :: Monoid w => (w -> w) -> StateWriter w a -> StateWriter w a
stateCensor f m = state (\e -> let (a, w) = runStateWriter m in (a, e <> f w))

{- 3. Используя монаду State, адаптируйте следующий императивный алгоритм для
вычисления длины списка:

def listLength(lst):
    len, acc = 0, lst
    while acc:
        len, acc = len + 1, acc[1:]
    return len
-}

-- | Функция, которая выполняет указанное вычисление до тех пор, пока условие не
--  перестанет быть истинным.
whileM_
  :: Monad m
  => 
  -- | Условие
     m Bool
  ->
  -- | Вычисление
     m a
  -> m ()
whileM_ mc ma = undefined

-- Для этой задачи снимается запрет на нетотальные функции, tail можно.
listLength :: [a] -> Int
listLength xs = undefined

task3 = listLength

{- 4. Дан код, который каким-то сложным образом использует монаду `State` для
вычисления n-ого элемента какой-то последовательности. Однако этот код слишком
простой (с точки зрения монад), чтобы нуждаться в `State`: на каждом шаге
применяется несложный автоморфизм. Напишите альтернативную версию `myFunction`,
которая использует монаду `Writer` с моноидом `Endo`. -}

encode = curry $ uncurry (*) . ((2 ^) *** (3 ^))

decode =
  uncurry (fix (\f y n -> bool (succ <$> f (y `div` 3) n) (n, 0) (y == 1)))
    . fix (\f y -> bool (succ <$> f (y `div` 2)) (y, 0) (odd y))

myFunction :: Int -> Int
myFunction i = fst $ decode $ execState (replicateM_ i step) 3
 where
  step :: State Integer ()
  step = do
    (n, m) <- gets decode
    let (n', m') = (m, (n + m) `mod` 10)
    put (encode n' m')

myFunction' :: Int -> Int
myFunction' i = undefined
 where
  step :: Writer (Endo Int) ()
  step = undefined

task4 = myFunction'

{- tell $ Endo $ \x -> let (n, m) = decode x
                           (n', m') = (m, n + m) in encode n' m' -}

{- 5. Дано несколько определений. Многие функции здесь делают одно и то же, но
различаются по строгости. Выделите из каждого набора ту функцию, которая кажется
лучшей, и объясните свой выбор.

Не всегда бывает понятно, какой код производительнее. В интерпретаторе это можно
проверить в ограниченной форме с помощью :set +s.

Подумайте, что произойдёт с очень большими или даже бесконечными Nat и
списками. -}

data Nat = Zero | Suc Nat deriving (Show)

natToInteger :: Nat -> Integer
natToInteger Zero    = 0
natToInteger (Suc n) = 1 + natToInteger n

natToInteger' :: Nat -> Integer
natToInteger' = go 0
 where
  go a Zero    = a
  go a (Suc n) = go (succ a) n

natToInteger'' :: Nat -> Integer
natToInteger'' = go 0
 where
  go a Zero    = a
  go a (Suc n) = go (succ a) (a `seq` n)

natToInteger''' :: Nat -> Integer
natToInteger''' = go 0
 where
  go a Zero    = a
  go a (Suc n) = go (a `seq` succ a) n

natToInteger'''' :: Nat -> Integer
natToInteger'''' = go 0
 where
  go a Zero    = a
  go a (Suc n) = a `seq` go (succ a) n

natToInteger''''' :: Nat -> Integer
natToInteger''''' = go 0
 where
  go a Zero    = a
  go a (Suc n) = seq a go (succ a) n

integerToNat :: Integer -> Nat
integerToNat i | i <= 0    = Zero
               | otherwise = Suc (integerToNat (pred i))

integerToNat' :: Integer -> Nat
integerToNat' = go Zero
 where
  go n i | i <= 0    = n
         | otherwise = go (Suc n) (pred i)

integerToNat'' :: Integer -> Nat
integerToNat'' = go Zero
 where
  go n i | i <= 0    = n
         | otherwise = n `seq` go (Suc n) (pred i)

evensAndOdds :: [a] -> ([a], [a])
evensAndOdds []       = ([], [])
evensAndOdds (x : xs) = let ~(pe, po) = evensAndOdds xs in (x : po, pe)

evensAndOdds' :: [a] -> ([a], [a])
evensAndOdds' = go [] []
 where
  go e o []       = (reverse o, reverse e)
  go e o (x : xs) = go (x : o) e xs

evensAndOdds'' :: [a] -> ([a], [a])
evensAndOdds'' = go [] []
 where
  go e o []       = (reverse o, reverse e)
  go e o (x : xs) = o `seq` go (x : o) e xs

evensAndOdds''' :: [a] -> ([a], [a])
evensAndOdds''' []       = ([], [])
evensAndOdds''' (x : xs) = let (pe, po) = evensAndOdds xs in (x : po, pe)

{- 6. Дан код на Си:

int fib(int n)
{
	int prev = 0;
	int curr = 1;
	while (n > 0) {
		int tmp = prev + curr;
		prev = curr;
		curr = tmp;
		--n;
	}
	return prev;
}

Хотим его оттранслировать в Haskell напрямую. Для этого воспользуемся `IORef` и
реализованной нами же выше функцией `whileM_`.

Проверяется задание не строго, так что можно вносить мелкие модификации для
упрощения кода.
-}

-- | Функция, которая уменьшает значение в заданной ячейке памяти на единицу.
decrementIORef :: Num n => IORef n -> IO ()
decrementIORef x = modifyIORef x (\x -> x - 1)

-- | Функция, вычисляющая n-ое число Фибоначчи с использованием `IORef`.
fib :: Int -> IO Integer
fib init = undefined

task6 = fib

{- 7. В модуле `System.Random` находятся функции для генерации случайных чисел.
По большей части `System` состоит из функций, которые требуют `IO` и обладают
какими-то побочными эффектами, но в этом конкретном модуле дополнительно даны
функции для генерации псевдослучайных чисел через детерминированные генераторы,
без использования системных источников случайных величин.

Функция `mkStdGen` возвращает проинициализированный генератор псевдослучайных
чисел по подаваемому первым аргументом зерну. Зерно (seed) генератора
(псевдо-)случайных чисел -- это некоторое числовое значение, которое определяет
начальное состояние генератора.

Функция `next` принимает на вход генератор, генерирует с его помощью случайное
число и возвращает пару из полученного числа и генератора с изменённым
состоянием. Все функции чистые, так что исходный генератор не меняется, его
можно вызывать сколько угодно раз и получать одно и то же число:

    let gen = mkStdGen 42
     in (fst <$> [next gen, next gen, next gen]) ==
        [1679910, 1679910, 1679910]

Более того, так как генераторы работают совершенно безо всякой случайности,
между перезапусками значения тоже будут сохраняться.

Чтобы получить череду случайных значений с таким интерфейсом, надо пользоваться
генератором как-то так:

    let (i1, g1) = next g0 in
    let (i2, g2) = next g1 in ...

Но это явно монадическое поведение. Реализуйте монаду, которая поддерживает
представленные ниже операции.
-}

-- Смените на любое удобное определение.
newtype MonadRandom a = MonadRandom (Identity a)
  deriving (Functor, Applicative, Monad)

-- | Функция, которая выполняет данный `MonadRandom`.
runMonadRandom
  ::
  -- | Выполняемая программа
     MonadRandom a
  ->
  -- | Зерно для начального значения генератора
     Int
  ->
  -- | Пара из полученного значения и состояния генератора на момент завершения
     (a, StdGen)
runMonadRandom m init = undefined

-- | Функция, которая инициализирует генератор случайных чисел в монаде,
-- используя заданное зерно.
reinit
  ::
  -- | Зерно
     Int -> MonadRandom ()
reinit = undefined

-- | Функция, которая оборачивает `next` в монаду.
nextInt :: MonadRandom Int
nextInt = undefined

-- | Функция, которая в монаде получает следующее число из генератора.
-- Посмотрите на список функций в классе `Random`.
nextValue :: Random a => MonadRandom a
nextValue = undefined

{- 8. Напишите программу, которая принимает как аргумент командной строки
количество k граней на кубиках, которые надо кинуть, а затем осуществляет
такие действия:

* Из системного генератора случайных чисел получает число n от 1 до k;
* Печатает на отдельной строке число n.
* Получает оттуда же серию чисел m_i (i от 1 до n), каждое из которых от 1 до k,
  и сразу же после получения очередного числа (до получения следующего) печатает
  в консоль (не в отдельной строке) m_i символов точки подряд.
* Производит перенос строки, а затем печатает одно число -- количество
  выведенных точек.

См. функции `randomIO` и `randomRIO`.

Примеры вывода:

    > :run task8 3
    1
    ..
    2
    > :run task8 10
    9
    .............................
    29
    > :run task8 10
    10
    ......................................................
    54
    > :run task8 5
    1
    ..
    2

-}
task8 :: IO ()
task8 = undefined

{- 9. Дан тип лямбда-термов. Используя монаду `Reader` и, в частности, функцию
`local`, реализуйте программу, которая возвращает множество свободных переменных
в лямбда-терме. Для этого в качестве контекста `Reader` надо использовать (и
поддерживать с помощью `local`) множество переменных, связанных снаружи. -}

{- Полезные функции Data.Set: empty, singleton, member, insert, delete. -}

data Lam a = Var a | App (Lam a) (Lam a) | Lam a (Lam a)
  deriving (Eq, Show, Generic)

task9 :: Ord a => Lam a -> S.Set a
task9 lam = runReader (go lam) S.empty
 where
  go :: Ord b => Lam b -> Reader (S.Set b) (S.Set b)
  go = undefined

{-
task9 :: Ord a => Lam a -> S.Set a
task9 lam = runReader
              (go lam)
              (\a -> False) -- изначально ни одна переменная не связана
  where go :: Ord b => Lam b -> Reader (b -> Bool) (S.Set b)
        go (Var a) = do isBound <- ask
                        pure $ if isBound a
                        then S.empty
                        else S.singleton a
        go (App a b) = (<>) <$> go a <*> go b
        go (Lam a m) = local (\f a' -> a == a' || f a') (go m)
-}

{- 10. Решите предыдущую задачу, но вместо монады `Reader` используйте `Writer`
с функцией `censor`. В качестве накапливаемого моноида держите множество
свободных переменных, которое надо вернуть. -}

task10 :: Ord a => Lam a -> S.Set a
task10 lam = execWriter (go lam)
 where
  go :: Ord b => Lam b -> Writer (S.Set b) ()
  go = undefined
