# FP2020Khalansky11

## Примеры ленивого исполнения кода

Этот раздел был разминкой к седьмому домашнему заданию, но мало кто её
заметил.

### Вопросы

Если возникнут сложности, то попробуйте прочитать все вопросы перед тем, как
подглядывать в ответы.

1.
```haskell
let { x = tail y; y = 0 : 1 : zipWith (+) x y} in take 10 y
```

2.
```haskell
import Data.Function (fix)
take 10 $ snd $ fix (\p -> (tail (snd p), 0 : 1 : uncurry (zipWith (+)) p))
```

3.
```haskell
import Data.Function (fix)
-- почему зависнет?
take 10 $ snd $ fix (\(x, y) -> (tail y, 0 : 1 : zipWith (+) x y))
```

4.
```haskell
import Data.Function (fix)
-- почему не зависнет?
take 10 $ snd $ fix $ \p -> let ~(x, y) = p in (tail y, 0 : 1 : zipWith (+) x y)
```

5.
```haskell
import Data.Function (fix)
-- а это почему не зависнет?
take 10 $ snd $ fix $ \p -> let (x, y) = p in (tail y, 0 : 1 : zipWith (+) x y)
```

6.
```haskell
data NonStrictPair a b = P1 a b
-- почему не зависнет?
take 10 $ (\(P1 x y) -> y) $ fix $ \p -> let (P1 x y) = p in P1 (tail y) (0 : 1 : zipWith (+) x y)
```

7.
```haskell
data HalfStrictPair a b = P2 !a b
-- зависнет!
take 10 $ (\(P2 x y) -> y) $ fix $ \p -> let (P2 x y) = p in P2 (tail y) (0 : 1 : zipWith (+) x y)
-- возможно ли это вообще починить?
```

8.
```haskell
data HalfStrictPair2 a b = P3 a !b
-- не зависнет.
take 10 $ (\(P3 x y) -> y) $ fix $ \p -> let (P3 x y) = p in P3 (tail y) (0 : 1 : zipWith (+) x y)
```

### Ответы

1. Это даже не вопрос, просто первые 10 чисел Фибоначчи.

2. И это не вопрос, а переформулирование предыдущего пункта через комбинатор
неподвижной точки. Оно будет раскрываться примерно так:

```haskell
take 10 $ snd $ fix f =
    {- чтобы найти WHNF (take 10 X), надо провести сопоставление X с
       образцом. Чтобы сопоставить (snd (fix f)) с образцом, надо
       сопоставить (fix f) с образцом. Чтобы сопоставить (fix f) с
       образцом, надо его довести до WHNF. Далее по определению fix: -}

take 10 $ snd $ f (fix f) =
    {- по определению f. Здесь я пишу `p = fix f`, а не просто произвожу
       подстановку, чтобы подчеркнуть, что оба `p` указывают на одно и то же,
       и если один `p` дойдёт до WHNF, то и второй тоже. Хотя тут это не
       имеет значения. -}

take 10 $ snd $ (tail (snd p), 0 : 1 : uncurry (zipWith (+)) p)[p = fix f] =
    -- можно довести (snd ...) до WHNF

take 10 $ (0 : 1 : uncurry (zipWith (+)) p)[p = fix f] =
    -- по определению take

0 : 1 : take 8 (uncurry (zipWith (+)) p)[p = fix f] =

0 : 1 : take 8 (uncurry (zipWith (+)) (fix f)) =
    {- по определению uncurry. Если бы определение было другим, то ход
       исполнения тоже изменился бы. Например, если бы uncurry
       производило явно сопоставление пары с образцом, то перед
       редуцированием uncurry пришлось бы найти WHNF у fix f. -}

0 : 1 : take 8 (zipWith (+) (fst p) (snd p))[p = fix f] =
    {- по определению zipWith, надо сопоставить первый аргумент с образцом.
       для этого надо найти WHNF (fst p), для чего нужно найти WHNF p. -}

0 : 1 : take 8 (zipWith (+) (fst p) (snd p))[p = f (fix f)] =
0 : 1 : take 8 (zipWith (+) (fst p) (snd p))
      [p = (tail (snd q), 0 : 1 : uncurry (zipWith (+)) q)][q = fix f] =
    {- сократим fst p, попутно назначая ячейке памяти, в которой лежит
       это значение, имя r. -}

0 : 1 : take 8 (zipWith (+) r (snd p))
      [p = (r, 0 : 1 : uncurry (zipWith (+)) q)]
      [r = tail (snd q)]
      [q = fix f] =
    -- чтобы найти WHNF r = tail (snd q), надо найти WHNF (snd q).

0 : 1 : take 8 (zipWith (+) r (snd p))
      [p = (r, 0 : 1 : uncurry (zipWith (+)) q)]
      [r = tail (snd q)]
      [q = f (fix f)] =

0 : 1 : take 8 (zipWith (+) r (snd p))
      [p = (r, 0 : 1 : uncurry (zipWith (+)) q)]
      [r = tail (snd q)]
      [q = (tail (snd s), 0 : 1 : uncurry (zipWith (+)) s)]
      [s = fix f] =

0 : 1 : take 8 (zipWith (+) r (snd p))
      [p = (r, 0 : 1 : uncurry (zipWith (+)) q)]
      [r = tail t]
      [q = (tail (snd s), t)]
      [t = 0 : 1 : uncurry (zipWith (+)) s]
      [s = fix f] =

0 : 1 : take 8 (zipWith (+) r (snd p))
      [p = (r, 0 : 1 : uncurry (zipWith (+)) q)]
      [r = t']
      [q = (tail (snd s), t)]
      [t = 0 : t']
      [t' = 1 : uncurry (zipWith (+)) s]
      [s = fix f] =

0 : 1 : take 8 (zipWith (+) t' (snd p))
      [p = (r, 0 : 1 : uncurry (zipWith (+)) q)]
      [q = (tail (snd s), t)]
      [t = 0 : t']
      [t' = 1 : uncurry (zipWith (+)) s]
      [s = fix f] =
    -- уберём лишние имена, которые теперь встречаются единожды

0 : 1 : take 8 (zipWith (+) t' (snd (r,
      0 : 1 : uncurry (zipWith (+)) (tail (snd s), 0 : t'))))
      [t' = 1 : uncurry (zipWith (+)) s]
      [s = fix f] =

0 : 1 : take 8 (zipWith (+) (1 : t'') (snd (r,
      0 : 1 : uncurry (zipWith (+)) (tail (snd s), 0 : t'))))
      [t' = 1 : t'']
      [t'' = uncurry (zipWith (+)) s]
      [s = fix f] =
    {- наконец-то довели первый аргумент zipWith до WHNF. По определению
       zipWith, раз первый аргумент непустой, надо искать WHNF второго.
       Благо, достаточно сократить snd. -}

0 : 1 : take 8 (zipWith (+) (1 : t'')
        (0 : 1 : uncurry (zipWith (+)) (tail (snd s), 0 : t')))
      [t' = 1 : t'']
      [t'' = uncurry (zipWith (+)) s]
      [s = fix f] =
    -- по определению zipWith

0 : 1 : take 8 ((1 + 0) : zipWith (+) t''
        (1 : uncurry (zipWith (+)) (tail (snd s), 0 : t')))
      [t' = 1 : t'']
      [t'' = uncurry (zipWith (+)) s]
      [s = fix f] =
    -- по определению take

0 : 1 : 1 + 0 : take 7 (zipWith (+) t''
        (1 : uncurry (zipWith (+)) (tail (snd s), 0 : t')))
      [t' = 1 : t'']
      [t'' = uncurry (zipWith (+)) s]
      [s = fix f] = ...
```

   Как видим, структура этого выражения вполне жизнеспособная и позволяет
   выдавать новые выражения.

3. Это решение не работает. Чтобы в этом убедиться, давайте потрассируем
исполнение.

```haskell
-- f = \(x, y) -> (tail y, 0 : 1 : zipWith (+) x y)
take 10 $ snd $ fix f =
  -- Как в прошлом пункте.
take 10 $ snd $ (\(x, y) -> (tail y, 0 : 1 : zipWith (+) x y)) (fix f) =
  {- Чтобы довести аргумент snd до WHNF, надо сократить редекс, однако для
  этого, в свою очередь, требуется сопоставить `fix f` с образцом `(x, y)`.
  Получается, надо довести `fix f` до WHNF. Раскрываем его снова. -}
take 10 $ snd $ (\(x, y) -> (tail y, 0 : 1 : zipWith (+) x y)) (f (fix f)) =

  {- здесь я ($) использую не как реальную функцию, а как синтаксис для
  удобства. -}
take 10 $ snd $ (\(x, y) -> (tail y, 0 : 1 : zipWith (+) x y)) $
                (\(x, y) -> (tail y, 0 : 1 : zipWith (+) x y)) (fix f) =
  -- Аналогичная ситуация: нужно довести fix f до WHNF, чтобы сделать шаг.
take 10 $ snd $ (\(x, y) -> (tail y, 0 : 1 : zipWith (+) x y)) $
                (\(x, y) -> (tail y, 0 : 1 : zipWith (+) x y)) $
                (\(x, y) -> (tail y, 0 : 1 : zipWith (+) x y)) (fix f) =
  -- и снова...
```
Как видно, ничего не выйдет: `fix f` будет раскрываться, пока память не
кончится, но так и не сможет сопоставиться с образцом.

4. `~`, как видно по следующему пункту, -- это отвлекающий манёвр.

5. 
```haskell
-- f = \p -> let (x, y) = p in (tail y, 0 : 1 : zipWith (+) x y)
take 10 $ snd $ fix f =

take 10 $ snd $ f (fix f) =

take 10 $ snd $ let (x, y) = fix f in (tail y, 0 : 1 : zipWith (+) x y) =
{- Заметим, например, что код
take 2 $ snd $ let (x, y) = undefined in (tail y, 0 : 1 : zipWith (+) x y)
нормально отработает и выдаст [0, 1]. Это означает, что вычислять связывания,
порождённые let-выражениями, не нужно, чтобы пользоваться телом. -}

-- примерно!
take 10 $ 0 : 1 : (zipWith (+) x y)[(x, y) = fix f] =

0 : 1 : take 8 (zipWith (+) x y)[(x, y) = fix f] =
{- Ясно, тут, чтобы получить следующий элемент, нужно довести zipWith _ _ _ до
WHNF, а для этого, по определению zipWith, надо сопоставить `x` с образцом. Для
этого придётся сопоставить `fix f` с образцом `(x, y)`, а для этого надо
довести `fix f` до WHNF. -}

0 : 1 : take 8 (zipWith (+) x y)[(x, y) = f (fix f)] =

0 : 1 : take 8 (zipWith (+) x y)
  [(x, y) = (\p -> let (x, y) = p in (tail y, 0 : 1 : zipWith (+) x y)) (fix f)]
  =

0 : 1 : take 8 (zipWith (+) x y)
  [(x, y) = let (x', y') = fix f in (tail y', 0 : 1 : zipWith (+) x' y')] =

0 : 1 : take 8 (zipWith (+) x y)
  [x = tail y']
  [y = 0 : 1 : zipWith (+) x' y']
  [(x', y') = fix f] =

0 : 1 : take 8 (zipWith (+) (tail y') (0 : 1 : zipWith (+) x' y'))
  [(x', y') = fix f] =
{- Чтобы довести `tail y'` до WHNF, надо довести `fix f` до WHNF и сопоставить
с образцом `(x', y')`. -}

0 : 1 : take 8 (zipWith (+) (tail y') (0 : 1 : zipWith (+) x' y'))
  [(x', y') = f (fix f)] =

0 : 1 : take 8 (zipWith (+) (tail y') (0 : 1 : zipWith (+) x' y'))
  [(x', y') = (\p -> let (x, y) = p in (tail y, 0 : 1 : zipWith (+) x y))
              (fix f)] =

0 : 1 : take 8 (zipWith (+) (tail y') (0 : 1 : zipWith (+) x' y'))
  [(x', y') = let (x, y) = fix f in (tail y, 0 : 1 : zipWith (+) x y)] =

0 : 1 : take 8 (zipWith (+) (tail y') (0 : 1 : zipWith (+) x' y'))
  [x' = tail y]
  [y' = 0 : 1 : zipWith (+) x y]
  [(x, y) = fix f] =

0 : 1 : take 8 (zipWith (+) (tail y') (0 : 1 : zipWith (+) (tail y) y'))
  [y' = 0 : 1 : zipWith (+) x y]
  [(x, y) = fix f] =

0 : 1 : take 8 (zipWith (+) (tail y') (0 : 1 : zipWith (+) (tail y) y'))
  [y' = 0 : y'']
  [y'' = 1 : zipWith (+) x y]
  [(x, y) = fix f] =

0 : 1 : take 8 (zipWith (+) y'' (0 : 1 : zipWith (+) (tail y) y'))
  [y' = 0 : y'']
  [y'' = 1 : zipWith (+) x y]
  [(x, y) = fix f] =

0 : 1 : take 8 (zipWith (+) y'' (0 : 1 : zipWith (+) (tail y) y'))
  [y' = 0 : y'']
  [y'' = 1 : y''']
  [y''' = zipWith (+) x y]
  [(x, y) = fix f] =

0 : 1 : take 8 (zipWith (+) (1 : y''') (0 : 1 : zipWith (+) (tail y) y'))
  [y' = 0 : y'']
  [y'' = 1 : y''']
  [y''' = zipWith (+) x y]
  [(x, y) = fix f] =
{- Наконец, можно сопоставить с образцом. -}

0 : 1 : take 8 ((1 + 0) : zipWith (+) y''' (1 : zipWith (+) (tail y) y'))
  [y' = 0 : y'']
  [y'' = 1 : y''']
  [y''' = zipWith (+) x y]
  [(x, y) = fix f] =

0 : 1 : (1 + 0) : take 7 (zipWith (+) y''' (1 : zipWith (+) (tail y) y'))
  [y' = 0 : y'']
  [y'' = 1 : y''']
  [y''' = zipWith (+) x y]
  [(x, y) = fix f] = -- ...
```
И так далее. Опять же, видно, что схема работоспособная.

6. Ни малейших отличий от предыдущего пункта, только тут вместо встроенных в
язык пар доморощенные.

7. Починить это можно только переставлением местами аргументов, чтобы было,
по факту, так же, как в последующем пункте, который не зависает. Проблема здесь
в том, что строгое хранение аргумента конструктора требует, чтобы экземпляр
считался находящимся в WHNF только тогда, когда первый аргумент тоже находится в
WHNF. Посмотрим, к чему это приведёт.

```haskell
-- f = \p -> let (P2 x y) = p in P2 (tail y) (0 : 1 : zipWith (+) x y)
take 10 $ (\(P2 x y) -> y) $ fix f =

take 10 $ (\(P2 x y) -> y) $ f (fix f) =

take 10 $ (\(P2 x y) -> y) $
  let (P2 x y) = fix f in P2 (tail y) (0 : 1 : zipWith (+) x y) =
{- В прошлых пунктах в этот момент мы уже могли сократить
`(\(P2 x y) -> y) $ _`. Здесь, однако, это не получится: тело `let` нельзя
сопоставить с образцом, так как оно не в WHNF: первый аргумент, `tail y`, не в
WHNF. Значит, придётся редуцировать его. -}

take 10 $ (\(P2 x y) -> y) $
  (P2 (tail y) (0 : 1 : zipWith (+) x y))
  [(P2 x y) = fix f] =

take 10 $ (\(P2 x y) -> y) $
  (P2 (tail y) (0 : 1 : zipWith (+) x y))
  [(P2 x y) = f (fix f)] =

take 10 $ (\(P2 x y) -> y) $
  (P2 (tail y) (0 : 1 : zipWith (+) x y))
  [(P2 x y) = let (P2 x' y') = fix f in P2 (tail y') (0 : 1 : zipWith (+) x' y')]
  =

take 10 $ (\(P2 x y) -> y) $
  (P2 (tail y) (0 : 1 : zipWith (+) x y))
  [(P2 x y) = P2 (tail y') (0 : 1 : zipWith (+) x' y')]
  [(P2 x' y') = fix f] =
{- Чтобы довести `tail y` до WHNF, нам нужно достать `y`. Но образец `P2 x y`
ещё не получится сопоставить, поскольку выражение справа, опять же, не в WHNF:
`tail y'` не в WHNF, а это первый аргумент, по которому наша структура данных
строга. Паттерн уже виден, так что дальше можно даже не пытаться, но просто
парочка контрольных редуцирований (если что, требуется довести y' до WHNF): -}

take 10 $ (\(P2 x y) -> y) $
  (P2 (tail y) (0 : 1 : zipWith (+) x y))
  [(P2 x y) = P2 (tail y') (0 : 1 : zipWith (+) x' y')]
  [(P2 x' y') = f (fix f)] =

take 10 $ (\(P2 x y) -> y) $
  (P2 (tail y) (0 : 1 : zipWith (+) x y))
  [(P2 x y) = P2 (tail y') (0 : 1 : zipWith (+) x' y')]
  [(P2 x' y') = let (P2 x'' y'') = fix f in P2 (tail y'') (0 : 1 : zipWith (+) x'' y'')] =

take 10 $ (\(P2 x y) -> y) $
  (P2 (tail y) (0 : 1 : zipWith (+) x y))
  [(P2 x y) = P2 (tail y') (0 : 1 : zipWith (+) x' y')]
  [(P2 x' y') = P2 (tail y'') (0 : 1 : zipWith (+) x'' y'')]
  [(P2 x'' y'') = fix f]
```
Видно, что дальше происходит всё то же самое.

8. Исполнение в этом пункте идентично тому, которое в пятом, поскольку
```haskell
0 : 1 : ...
```
уже явно в WHNF, так что никакой погоды не делает то, что `P3` должен быть
строгим по второму аргументу: у нас этот аргумент во все моменты и так в WHNF.

## Процедура сдачи

* `git checkout -b solution`
* Реализовываете задания из `src/Lib.hs`
* `hlint src/Lib.hs`
* `stack test` (`cabal test`, если ставили не через `stack`)
* Если в коде встречаются `head`, `tail`, `null`, `isJust`, `isNothing`,
  `fromJust` или что-нибудь в таком духе, добавляете комментарий к каждому
  их использованию с пояснением, почему сопоставление с образцом было бы
  более плохим решением. Бесконечные списки являются исключением: на них
  можно делать хоть `head`, хоть `tail`, хоть `!!` без объяснений.
* `git commit`
* `git push -u origin solution`
* Pull request `solution` -> `master`, меня (`dkhalansky`) в проверяющие,
  фамилия/имя где-то в названии PR.

Коммиты новые можно добавлять сколько угодно, смотреть не буду, пока не
попросите.
