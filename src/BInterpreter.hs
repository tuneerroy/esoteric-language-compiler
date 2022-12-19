{-# LANGUAGE TemplateHaskell #-}

module BInterpreter where

import BSyntax (BInstruction (..))
import Control.Lens (Ixed (ix), makeLenses, (%~), (&), (.~), (^.), (^?))
import Control.Monad.State (MonadState (get, put), StateT (runStateT), execStateT)
import Control.Monad.Trans (MonadTrans (..))
import Data.Char (chr)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe (fromMaybe)
import Data.Word (Word8)
import MonadReadWrite (MonadReadWrite (..))

-- | Internal state of a brainfuck program
data BStore = BStore
  { _ptr :: Int,
    _heap :: Map Int Word8
  }

makeLenses ''BStore

-- | Initial internal state of a brainfuck program
initStore :: BStore
initStore = BStore 0 Map.empty

-- | A monad, that when run, executes the program
toProgramState :: forall m. (MonadState BStore m, MonadReadWrite m) => [BInstruction] -> m ()
toProgramState [] = return ()
toProgramState program@(instr : instrs) = do
  store <- get
  case instr of
    IncrPtr -> do
      put (store & ptr %~ (+) 1)
      toProgramState instrs
    DecrPtr -> do
      put (store & ptr %~ (-) 1)
      toProgramState instrs
    IncrByte -> do
      put (store & heap %~ Map.alter (adjustByte $ (+) 1) (_ptr store))
      toProgramState instrs
    DecrByte -> do
      put (store & heap %~ Map.alter (adjustByte $ (-) 1) (_ptr store))
      toProgramState instrs
    Output -> do
      writeString [chr $ fromEnum $ Map.findWithDefault 0 (_ptr store) (_heap store)]
      toProgramState instrs
    Input -> do
      v <- readChar
      put (store & heap %~ Map.insert (_ptr store) (toEnum $ fromEnum v))
      toProgramState instrs
    While s -> do
      case Map.findWithDefault 0 (_ptr store) (_heap store) of
        0 -> toProgramState instrs
        _ -> toProgramState (s ++ program)
  where
    adjustByte :: (Word8 -> Word8) -> Maybe Word8 -> Maybe Word8
    adjustByte f x = Just $ fromMaybe 0 x & f

execProgram :: MonadReadWrite m => [BInstruction] -> m BStore
execProgram program = execStateT (toProgramState program) initStore

-- | Execute the program in IO
execProgramIO :: [BInstruction] -> IO BStore
execProgramIO = execProgram