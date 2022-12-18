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

runProgram :: forall m. (ProgramState BStore m, MonadReadWrite m, MonadError BError m) => Program BInstruction -> m ()
runProgram program = do
  (store, pc) <- get
  instr <- case program ^? ix pc of
    Nothing -> throwError ProgramOutOfBounds
    Just wi -> return wi
  case instr of
    IncrPtr -> do
      put (store & ptr %~ (+) 1, pc)
    DecrPtr -> do
      put (store & ptr %~ (-) 1, pc)
    IncrByte -> do
      put (store & heap %~ Map.alter (adjustByte $ (+) 1) (_ptr store), pc)
    DecrByte -> do
      put (store & heap %~ Map.alter (adjustByte $ (-) 1) (_ptr store), pc)
    Output ->
      let v = Map.findWithDefault 0 (_ptr store) (_heap store)
       in writeString [fromEnum v & chr]
    Input -> do
      v <- readChar
      put (store & heap %~ Map.insert (_ptr store) (fromEnum v & toEnum), pc)
    WhileStart v -> case Map.findWithDefault 0 (_ptr store) (_heap store) of
      0 -> put (store, v)
      _ -> put (store, pc)
    WhileEnd v -> put (store, v)
  put (store, pc + 1)
  unless (pc == snd (bounds program)) $ do
    runProgram program
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

instance MonadReadWrite m => MonadReadWrite (StateT s (ExceptT e m)) where
  readChar :: StateT s (ExceptT e m) Char
  readChar = lift readChar
  writeString :: String -> StateT s (ExceptT e m) ()
  writeString = lift . writeString

type ProgramMonad = StateT (BStore, Int) (ExceptT BError IO)

-- x = runExceptT $ runStateT performExampleProgram (initStore, 0)