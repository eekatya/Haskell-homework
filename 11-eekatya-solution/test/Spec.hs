{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

import Control.Arrow (Arrow ((***)), (&&&))
import Control.Monad (join, replicateM, replicateM_)
import Control.SimpleMonad
import Data.Bool (bool)
import Data.Function (fix, on)
import Data.Functor ((<&>))
import Data.IORef
import Data.List (group, sort)
import qualified Data.Set as S
import Generic.Random (genericArbitrary', genericArbitraryU, (%))
import Lib
import Test.HUnit
import Test.QuickCheck

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

qcArgs = stdArgs {maxSize = 20, chatty = False}

quickTestToTestCase descr test = TestCase $ do
  result <- test
  let message = "ожидается, что " ++ descr ++ ", но\n" ++ output result
  if isSuccess result
    then pure ()
    else assertBool message False

-- Task 1 ---------------------------------------------------------------------

task1Test =
  TestList
    [ TestCase $
        assertEqual "все функции StateReader разом, " True $
          flip runStateReader [3, 4] $ do
            a <- stateLocal ([1, 2] <>) $ stateReader (sum :: [Int] -> Int)
            b <- stateAsks (length :: [Int] -> Int)
            c <- stateLocal (const [30]) $ (stateAsk :: StateReader [Int] [Int])
            pure $ a + b + product c == 42
    ]

-- Task 2 ---------------------------------------------------------------------

task2Test =
  TestList
    [ TestCase $
        assertEqual "все функции StateWriter разом, " ("xax", "xyzzyx") $
          runStateWriter $ do
            a <- stateWriter ("y", "x")
            stateTell "y"
            stateTell "z"
            ((), w) <- (stateListen $
              stateTell "z" :: StateWriter String ((), String))
            () <- stateCensor (take 1) $ stateTell "yy"
            (a, s) <-
              stateListens (<> "ax") $
                stateWriter ("y", "x")
            pure $ if a == "y" then s else "xyx"
    ]

-- Task 3 ---------------------------------------------------------------------

task3Test =
  TestList
    [ TestCase $
        assertEqual
          "whileM_ завершается на первом шаге, когда надо, "
          (Just ())
          (whileM_ (Just False) Nothing),
      TestCase $
        assertEqual
          "whileM_ выполняет код в цикле, "
          1
          (execState (whileM_ (get <&> (<= 0)) (modify (+ 1))) (-3)),
      quickTestToTestCase "listLength == length" $
        quickCheckWithResult qcArgs $
          \(l :: [()]) -> length l === task3 l
    ]

-- Task 4 ---------------------------------------------------------------------

task4Test =
  TestList
    [ quickTestToTestCase "myFunction == myFunction'" $
        quickCheckWithResult qcArgs $ \i -> myFunction i == task4 i
    ]

encode = curry $ uncurry (*) . ((2 ^) *** (3 ^))

decode =
  uncurry (fix (\f y n -> bool (succ <$> f (y `div` 3) n) (n, 0) (y == 1)))
    . fix (\f y -> bool (succ <$> f (y `div` 2)) (y, 0) (odd y))

myFunction :: Int -> Int
myFunction i = fst $ decode $ execState (replicateM_ i step) 3
  where
    step :: Control.SimpleMonad.State Integer ()
    step = do
      (n, m) <- gets decode
      let (n', m') = (m, (n + m) `mod` 10)
      put (encode n' m')

-- Task 5 ---------------------------------------------------------------------

task5Test = TestList []

-- Task 6 ---------------------------------------------------------------------

task6Test =
  TestList
    [ TestCase $ do
        ref <- newIORef 5
        decrementIORef ref
        v' <- readIORef ref
        assertEqual "decrementIORef работает" 4 v',
      TestCase $ do
        fibs <- mapM task6 [0 .. 15]
        let trueFibs = [0, 1, 1, 2, 3, 5, 8, 13, 21, 34, 55, 89, 144, 233, 377, 610]
        assertEqual "fib считает числа Фибоначчи" trueFibs fibs
    ]

-- Task 7 ---------------------------------------------------------------------

task7Test =
  TestList
    [ TestCase $ assertBool "nextInt меняет состояние"
      (let ((v, v'), _) = runMonadRandom ((,) <$> nextInt <*> nextInt) 42
        in v /= v'),
      let ((v, v'), _) = runMonadRandom (((,) <$> nextInt <* nextInt)
                                              <*> (reinit 42 *> nextInt)) 42 in
      TestCase $ assertEqual "reinit инициализирует состояние" v v',
      TestCase $ let (i, i', i'', d, d', d'') = fst $ flip runMonadRandom 42 $
                                                do i :: Int <- nextValue
                                                   i' :: Int <- nextValue
                                                   i'' :: Int <- nextInt
                                                   d :: Double <- nextValue
                                                   d' :: Double <- nextValue
                                                   d'' :: Double <- nextValue
                                                   pure (i, i', i'', d, d', d'')
        in do assertBool "i /= i'" (i /= i')
              assertBool "i' /= i''" (i' /= i'')
              assertBool "i /= i''" (i /= i'')
              assertBool "d /= d''" (d /= d'')
              assertBool "d /= d'" (d /= d')
              assertBool "d' /= d''" (d' /= d'')
    ]

-- Task 8 ---------------------------------------------------------------------

-- Это не то чтобы тестируется изнутри Хаскелля, особенно с HUnit, простите.
task8Test = TestList []

-- Task 9 ---------------------------------------------------------------------

instance Arbitrary a => Arbitrary (Lam a) where
  arbitrary = genericArbitraryU

validateFreeVars :: (Lam (Small Int) -> S.Set (Small Int)) -> Property
validateFreeVars f = property $ \lam -> f lam === f' (pure False) lam mempty
  where
    f' :: Ord b => (b -> Bool) -> Lam b -> S.Set b -> S.Set b
    f' b (Var a) = if b a then id else (<>) (S.singleton a)
    f' b (App m n) = f' b m . f' b n
    f' b (Lam a m) = f' (\x -> x == a || b x) m

task9Test =
  TestList
    [ quickTestToTestCase "task9 находит ровно свободные переменные" $
        quickCheckWithResult qcArgs (validateFreeVars task9)
    ]

-- Task 10 --------------------------------------------------------------------

task10Test =
  TestList
    [ quickTestToTestCase "task10 находит ровно свободные переменные" $
        quickCheckWithResult qcArgs (validateFreeVars task10)
    ]
