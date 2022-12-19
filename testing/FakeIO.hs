module FakeIO (outputOf, finalStateOf, FakeState (..)) where

import Control.Monad (ap, liftM)
import Data.Sequence (Seq, (|>))
import Data.Sequence qualified as Seq
import WStepper (MonadReadWrite (..))

-- A copy of the state monad to prevent typeclass interference
newtype State' s a = S (s -> (a, s))

runState' :: State' s a -> s -> (a, s)
runState' (S f) = f

{-
There are two other ways of evaluating the state monad. The first only
returns the final result,
-}

evalState' :: State' s a -> s -> a
evalState' (S f) x = fst $ f x

{-
and the second only returns the final state.
-}

execState' :: State' s a -> s -> s
execState' (S f) x = snd $ f x

instance Monad (State' s) where
  return :: a -> State' s a
  return x = S (x,)

  (>>=) :: State' s a -> (a -> State' s b) -> State' s b
  (S st) >>= f = S (\x -> case st x of (a, s) -> runState' (f a) s)

instance Functor (State' s) where
  fmap = liftM

instance Applicative (State' s) where
  pure = return
  (<*>) = ap

-- | The actual fake state
data FakeState = FakeState
  { input :: String,
    output :: String -> String
  }

type FakeIO = State' FakeState

get' :: State' s s
get' = S $ \s -> (s, s)

put' :: s -> State' s ()
put' s' = S $ const ((), s')

instance MonadReadWrite FakeIO where
  readChar = do
    state <- get'
    let (x : xs) = input state
    put' $ state {input = xs}
    return x
  writeString s = do
    state <- get'
    put' $ state {output = output state . (s <>)}

ofInput :: String -> FakeState
ofInput = flip FakeState id

outputOf :: FakeIO (Either e s) -> String -> Either e String
outputOf fakeIO input = e >> return (output state [])
  where
    (e, state) = runState' fakeIO (ofInput input)

finalStateOf :: FakeIO (Either e s) -> String -> Either e s
finalStateOf fakeIO input = e
  where
    (e, state) = runState' fakeIO (ofInput input)