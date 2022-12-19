{-# LANGUAGE TemplateHaskell #-}

module BStepper where

import BSyntax (BInstruction (..))
import Control.Lens (Ixed (ix), makeLenses, (%~), (&), (.~), (^.), (^?))
import Control.Monad (forM_, unless, void, when)
import Control.Monad.Except (ExceptT, MonadError (..), runExceptT)
import Control.Monad.State (MonadState (get, put), StateT (runStateT))
import Control.Monad.State.Lazy (StateT, modify)
import Control.Monad.Trans (MonadTrans (..))
import Data.Char (chr)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe (fromMaybe)
import Data.Word (Word8)
import GHC.Arr (bounds)
import Program (Program, ProgramState)

class Monad m => MonadReadWrite m where
  readChar :: m Char
  writeString :: String -> m ()

data BStore = BStore
  { _ptr :: Int,
    _heap :: Map Int Word8
  }

makeLenses ''BStore

initStore :: BStore
initStore = BStore 0 Map.empty

data BError = MissingMatchingBracket | ProgramOutOfBounds

-- TODO: maybe extract runProgram out? everything dealing with program counter

runProgram :: forall m. (MonadState BStore m, MonadReadWrite m, MonadError BError m) => [BInstruction] -> m ()
runProgram [] = return ()
runProgram program@(instr : instrs) = do
  store <- get
  case instr of
    IncrPtr -> do
      put (store & ptr %~ (+) 1)
      runProgram instrs
    DecrPtr -> do
      put (store & ptr %~ (-) 1)
      runProgram instrs
    IncrByte -> do
      put (store & heap %~ Map.alter (adjustByte $ (+) 1) (_ptr store))
      runProgram instrs
    DecrByte -> do
      put (store & heap %~ Map.alter (adjustByte $ (-) 1) (_ptr store))
      runProgram instrs
    Output -> do
      writeString [chr $ fromEnum $ Map.findWithDefault 0 (_ptr store) (_heap store)]
      runProgram instrs
    Input -> do
      v <- readChar
      put (store & heap %~ Map.insert (_ptr store) (toEnum $ fromEnum v))
      runProgram instrs
    While s -> do
      case Map.findWithDefault 0 (_ptr store) (_heap store) of
        0 -> runProgram instrs
        _ -> runProgram (s ++ program)
  where
    adjustByte :: (Word8 -> Word8) -> Maybe Word8 -> Maybe Word8
    adjustByte f x = Just $ fromMaybe 0 x & f

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

instance MonadReadWrite m => MonadReadWrite (StateT s m) where
  readChar :: StateT s m Char
  readChar = lift readChar
  writeString :: String -> StateT s m ()
  writeString = lift . writeString

-- x = runExceptT $ runStateT performExampleProgram (initStore, 0)