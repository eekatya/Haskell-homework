{-# LANGUAGE DeriveFunctor #-}

module Control.SimpleMonad where

import Control.Monad (ap)

-- -----------------------------------------------------------------------------
newtype Reader e a = Reader {runReader :: e -> a}
  deriving (Functor)

instance Applicative (Reader e) where
  pure a = Reader $ const a
  (<*>) = ap

instance Monad (Reader e) where
    return = pure
    {- Попробуйте стереть всё это определение и восстановить его самостоятельно
       по типовым дырам. -}
    ma >>= g = Reader $ \e -> runReader (g (runReader ma e)) e

reader :: (e -> a) -> Reader e a
reader = Reader

ask :: Reader e e
ask = asks id -- Reader id

local :: (e -> e) -> Reader e a -> Reader e a
local f m = Reader $ \e -> let e' = f e
                               g = runReader m
                            in g e'
-- reader $ runReader m . f

asks :: (e -> a) -> Reader e a
asks = reader

-- -----------------------------------------------------------------------------
newtype Writer w a = Writer {runWriter :: (a, w)}
  deriving (Show, Eq, Ord, Functor)

instance Monoid w => Applicative (Writer w) where
    pure a = Writer (a, mempty)
    (<*>) = ap

instance Monoid w => Monad (Writer w) where
    return = pure
    ma >>= g = let ~(a, wa) = runWriter ma
                   ~(b, wb) = runWriter (g a)
                in Writer (b, wa <> wb)

execWriter :: Monoid w => Writer w a -> w
execWriter = snd . runWriter

writer :: Monoid w => (a, w) -> Writer w a
writer = Writer

tell :: Monoid w => w -> Writer w ()
tell w = writer ((), w)

listen :: Monoid w => Writer w a -> Writer w (a, w)
listen m = writer $ let (a, w) = runWriter m in ((a, w), w)
-- listen = listens id

listens :: Monoid w => (w -> b) -> Writer w a -> Writer w (a, b)
listens f m = writer $ let (a, w) = runWriter m in ((a, f w), w)

-- Пока назначение этой функции не должно быть понятно.
pass :: Monoid w => Writer w (a, w -> w) -> Writer w a
pass m = writer $ let ((a, f), w') = runWriter m in (a, f w')

censor :: Monoid w => (w -> w) -> Writer w a -> Writer w a
censor f m = writer $ let (a, w) = runWriter m in (a, f w)

-- -----------------------------------------------------------------------------
newtype State s a = State {runState :: s -> (a, s)}
  deriving (Functor)

instance Applicative (State s) where
  pure a = State $ \s -> (a, s)
  (<*>) = ap

instance Monad (State s) where
  return = pure
  -- Почти определение с лекции
  fa >>= g = State $ \s -> let (a, s') = runState fa s
                               fb = g a
                            in runState fb s'

state :: (s -> (a, s)) -> State s a
state = State

get :: State s s
get = state $ \s -> (s, s)

{-
get = gets id
-}

put :: s -> State s ()
put s = state $ \_ -> ((), s)

evalState :: State s a -> s -> a
evalState m s = fst (runState m s)

execState :: State s a -> s -> s
execState m s = snd (runState m s)

gets :: (s -> a) -> State s a
gets f = state $ \s -> (f s, s)

{-
gets f = do s <- get
            return $ f s

gets f = f <$> get
-}

mapState :: ((a, s) -> (b, s)) -> State s a -> State s b
mapState f m = state $ f . runState m

modify :: (s -> s) -> State s ()
modify f = state $ \s -> ((), f s)

modify' :: (s -> s) -> State s ()
modify' f = do
  s <- get
  put $! s

withState :: (s -> s) -> State s a -> State s a
withState f m = modify f >> m
