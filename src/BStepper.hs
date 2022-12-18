{-# LANGUAGE TemplateHaskell #-}

module WStepper where

import BSyntax
import Control.Lens (Ixed (ix), makeLenses, (%~), (&), (.~), (^.), (^?))
import Control.Monad (forM_, unless, void, when)
import Control.Monad.Except (ExceptT, MonadError (..), runExceptT)
import Control.Monad.State (MonadState (get, put), StateT (runStateT))
import Control.Monad.State.Lazy (StateT, modify)
import Control.Monad.Trans (MonadTrans (..))
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Word (Word8)
import GHC.Arr (Ix (range), (!))

class Monad m => MonadReadWrite m where
  readChar :: m Char
  writeString :: String -> m ()

data BStore = BStore
  { _ptr :: Int,
    _heap :: Map Int Word8
  }

makeLenses ''BStore

initStore :: BStore
initStore = BStore [] [] Map.empty

data WError
  = ProgramOutOfBounds
  | ValStackEmpty
  | CallStackEmpty
  | StackOutOfBounds
  | HeapKeyNotFound
  | LabelFound

runProgram :: forall m. (ProgramState BStore m, MonadReadWrite m, MonadError WError m) => Program BInstruction -> m ()
runProgram program = do
  (store, pc) <- get
  instr <- case program ^? ix pc of
    Nothing -> throwError ProgramOutOfBounds
    Just wi -> return wi
  case instr of {}
  -- IncrPtr -> undefined
  -- DecrPtr -> undefined
  -- IncrByte -> undefined
  -- DecrByte -> undefined
  -- Output -> undefined
  -- Input -> undefined
  -- WhileStart -> undefined
  -- WhileEnd -> undefined
  -- InputChar -> do
  --   v <- fromEnum <$> readChar
  --   push v
  -- InputNum -> do
  --   v <- read . pure <$> readChar
  --   push v
  -- OutputChar -> do
  --   n <- pop
  --   writeString [toEnum n]
  -- OutputNum -> do
  --   n <- pop
  --   writeString [toEnum n]
  -- Push n -> push n
  -- Dup -> do
  --   n <- pop
  --   push n
  --   push n
  -- Swap -> do
  --   n <- pop
  --   m <- pop
  --   push m
  --   push n
  -- Discard -> void pop
  -- Copy i -> do
  --   n <- case store ^. valStack ^? ix i of
  --     Nothing -> throwError StackOutOfBounds
  --     Just n -> pure n
  --   push n
  -- Slide n -> do
  --   top <- pop
  --   forM_ [0 .. n] $ const pop
  --   push top
  -- Arith wb -> do
  --   b <- pop
  --   a <- pop
  --   let op = case wb of
  --         Add -> (+)
  --         Sub -> (-)
  --         Mul -> (*)
  --         Div -> div
  --         Mod -> mod
  --   push $ op a b
  -- Label n -> throwError LabelFound
  -- Call n -> put (store & callStack %~ (pc :), n)
  -- Branch wc n -> do
  --   top <- pop
  --   let jump = case wc of
  --         Any -> True
  --         Zero -> top == 0
  --         Neg -> top < 0
  --   when jump $ put (store, n - 1)
  -- Return -> do
  --   case store ^. callStack of
  --     [] -> throwError CallStackEmpty
  --     n : ns -> put (store & callStack .~ ns, n)
  -- End -> return ()
  -- Store -> do
  --   val <- pop
  --   addr <- pop
  --   put (store & heap %~ Map.insert addr val, pc)
  -- Retrieve -> do
  --   addr <- pop
  --   case store ^. heap & Map.lookup addr of
  --     Nothing -> throwError HeapKeyNotFound
  --     Just n -> push n
  unless (instr == End) $ do
    put (store, pc + 1)
    runProgram program
  where
    pop :: m Int
    pop = do
      (store, pc) <- get
      case store ^. valStack of
        [] -> throwError ValStackEmpty
        n : ns -> do
          put (store & valStack .~ ns, pc)
          return n

    push :: Int -> m ()
    push n = do
      (store, pc) <- get
      put (store & valStack %~ (n :), pc)

-- TODO: gotta move this stuff to a separate file later
-- too much repetitive code

instance MonadReadWrite IO where
  readChar :: IO Char
  readChar = getChar
  writeString :: String -> IO ()
  writeString = putStrLn

instance MonadReadWrite m => MonadReadWrite (ExceptT e m) where
  readChar :: ExceptT e m Char
  readChar = lift readChar
  writeString :: String -> ExceptT e m ()
  writeString = lift . writeString

instance MonadReadWrite m => MonadReadWrite (StateT s (ExceptT e m)) where
  readChar :: StateT s (ExceptT e m) Char
  readChar = lift readChar
  writeString :: String -> StateT s (ExceptT e m) ()
  writeString = lift . writeString

type ProgramMonad = (StateT (BStore, Int) (ExceptT WError IO))

x = runExceptT $ runStateT performExampleProgram (initStore, 0)