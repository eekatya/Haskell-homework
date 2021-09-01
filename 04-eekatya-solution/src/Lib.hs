module Lib
  ( task1
  , task2
  , task2'
  , task3
  , (+???)
  , task5
  , task6
  , task7
  , task8
  )
where
import           Data.Function
{- Поиграйте с интерпретатором. Убедитесь, что понимаете результаты исполнения.

* 5 + 3
* 5 + (-3)
* (+) 5 (-3)
* (/) 5.0 (-3.0)
* (5.0 /) (-3.0)
* (/ 5.0) (-3.0)
* :t (==)
* 'c' == True
* ``str'' == True
* :t succ
* succ 'c'
* succ 15
* :type id
* :t id
* :info id
* :i id
* :i (+)
* :i Num
* :t \x -> x + x
* :doc id
* (\x -> x + x) 3
* Последовательно 3, it, it + it, it + it
* :t (5, True)
* :t fst
* fst (5, True)
* snd (5, True)
* if 3 == 3 then 15 else 14
* if 3 == 3 then 15 else True
* let x = 15 in x + x
* :t error
* if True then 4 + error ``fail miserably'' else 5
* Сначала import Data.Function, затем
  Data.Function.fix error
* fix (\f n -> if n <= 0 then 0 else succ (f (n - 1))) 100
* Создайте файл ``H.hs'' в этой же директории и поместите в него строку
  val = 3
  Затем из интерпретатора:
    :load H
    val
    it
  Не закрывая интерпретатор, измените значение val. В интерпретаторе
  сделайте
    :reload
    val

Изучите базовые библиотечные функции и некоторые их комбинации (обычно
достаточно узнать тип) и найдите по возможности их аналоги в лямбда-исчислении:

* id
* const
* flip
* flip const
* (.)
* curry
* swap
* uncurry
* uncurry const
* uncurry (flip const)
* swap

Всё, что выше, не оценивается.
-}

{- Оттранслируйте в Хаскелль лямбда-терм
   λn s z. snd (n (λp. pair (s (fst p)) (fst p)) (pair z z)) -}
task1 n s z = snd (n (\p -> pair (s (fst p)) (fst p)) (pair z z))
 where
  pair a b t = t a b
  fst p = p const
  snd p = p (\x y -> y)

{- Реализуйте взаимно-рекурсивную пару функций:
   isEven(n) = True, если n = 0
               isOdd(n + 1), если n < 0
               isOdd(n - 1), если n > 0
   isOdd(n) = False, если n = 0
              isEven(n + 1), если n < 0
              isEven(n - 1), если n > 0
   При реализации этой функции используйте guard'ы (см. лекцию). -}
isEven :: Integer -> Bool
isEven n | n == 0 = True
         | n < 0  = isOdd (n + 1)
         | n > 0  = isOdd (n - 1)


isOdd :: Integer -> Bool
isOdd n | n == 0 = False
        | n < 0  = isEven (n + 1)
        | n > 0  = isEven (n - 1)

task2 = isEven

task2' = isOdd

{- T(n) = 1, если n = 1
          T(n/2), если n делится на 2 (см. предыдущее задание)
          T(3 * n + 1) в противном случае.
   При реализации этой функции используйте guard'ы.

   Для целочисленного деления используется не /, а функция `div`.
-}
task3 :: Integer -> Integer
task3 n | n == 1   = 1
        | isOdd n  = task3 (3 * n + 1)
        | isEven n = task3 (n `div` 2)
{- Левоассоциативный оператор с шестым приоритетом, который
   сравнивает два аргумента и возвращает их сумму, если левый
   больше правого, и произведение в противном случае. -}
(+???) :: Integer -> Integer -> Integer
a +??? b | a > b  = a + b
         | b >= a = a * b

task4 = (+???)

-- | Функция, которая подсчитывает количество битов в двоичном представлении
--     модуля данного числа, а также находит количество битов,
--     установленных в единицу.
task5 :: Integer -> (Integer, Integer)
task5 n = undefined

-- Реализуйте функцию.
task6 :: (a -> b) -> (a -> c) -> a -> (b, c)
task6 f x y = (f y, x y)

-- Реализуйте функцию.
task7 :: (a -> b -> c) -> (a -> b) -> a -> (a, b, c)
task7 f x y = (y, x y, f y (x y))

{- Найдите двойной факториал, то есть произведение всех чисел меньше данного,
   имеющих ту же чётность. Например, 6!! = 6 * 4 * 2 = 48.
   Сделайте это через комбинатор неподвижной точки (Data.Function.fix). -}
task8 :: Integer -> Integer
task8 = fix (\f n -> if n == 0 || n == 1 then 1 else n * f (n - 2))
