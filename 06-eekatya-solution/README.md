# FP2020Khalansky06

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

## Разминка по классам типов

### Задания

#### Цвет

Задайте тип `Color`, который параметризуется типом `a` и содержит сведения о
том, сколько в данном цвете красного, зелёного и синего компонентов. Поля
должны быть именованы. Каждое поле должно быть типа `a` и находиться в
`Color` строго, то есть форсирование экземпляра `Color` до головной нормальной
формы должно форсировать и поля.

Определите синоним типа `FloatColor`, который задаёт цвет как набор из трёх
чисел с плавающей точкой. Установим соглашение, что корректные значения
каждой из компонент `FloatColor` находятся в промежутке от `0.0` до `1.0`.

#### Цвет-функтор

Сделайте цвет функтором.

#### Цвет-число

Реализуйте `Num` для тех цветов, в которых компоненты кодируются числами.

#### Цветное

Задайте класс типов `Colored` над типами с kind-ом `*` и операциями

```haskell
fg_color :: Colored a => a -> FloatColor
bg_color :: Colored a => a -> FloatColor
colors :: Colored a => a -> (FloatColor, FloatColor)
```

`colors` должен возвращать `(fg_color, bg_color)`.

Сделайте стандартные определения для всех операций и сообщите Haskell, что
минимальное разумное определение содержит либо `fg_color` и `bg_color`, либо
`colors`.

#### Цветные числа

Сделайте обёртку `newtype ColorNumber` над произвольным типом `a` и реализуйте
для неё экземпляр `Colored` в случае, когда `a` является целочисленным типом,
таким образом: красная компонента фона пропорциональна остатку от деления `a` на
три, зелёная — остатку от деления на семь, синяя — остатку от деления на пять;
цвет

### Решения

#### Цвет

```haskell
data Color a = Color
             { red   :: !a
             , green :: !a
             , blue  :: !a
             }

type FloatColor = Color Float
```

#### Цвет-функтор

```haskell
instance Functor Color where
  fmap f c = Color { red = f (red c)
                   , green = f (green c)
                   , blue = f (blue c)
                   }
```

#### Цвет-число

```haskell
{- Если происходящее непонятно, найдите с помощью Hoogle исходники liftA2 и
   разберитесь, почему всё работает. -}
instance Applicative Color where
  pure c = Color c c c
  cf <*> cc = Color { red = red cf (red cc)
                    , green = green cf (green cc)
                    , blue = blue cf (blue cc)
                    }

instance Num a => Num (Color a) where
  (+) = liftA2 (+)
  (-) = liftA2 (-)
  (*) = liftA2 (*)
  negate = fmap negate
  abs = fmap abs
  signum = fmap signum
  fromInteger = pure . fromInteger
```

#### Цветное

```haskell
class Colored a where -- a :: *
  fg_color :: a -> FloatColor
  fg_color = fst colors
  bg_color :: a -> FloatColor
  bg_color = snd colors
  colors :: a -> (FloatColor, FloatColor)
  colors = (fg_color, bg_color)
  {-# MINIMAL colors | fg_color, bg_color #-}
```

#### Цветные числа

```haskell
newtype ColorNumber a = ColorNumber a

instance Integral a => Colored (ColorNumber a) where
  fg_color x = 1.0 - bg_color x
  bg_color x = Color
               { red = fromIntegral (x `mod` 3) / 3.0
               , green = fromIntegral (x `mod` 7) / 7.0
               , blue = fromIntegral (x `mod` 5) / 5.0
               }
```
