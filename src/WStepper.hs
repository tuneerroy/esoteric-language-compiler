module WStepper where

import Control.Monad.Except (MonadError(..))
import Control.Monad.State (MonadState)
import Data.Map (Map)
import Program
import WSyntax (WInstruction (..))
import GHC.Arr ((!))
import Control.Monad.RWS (MonadState(get, put))

class IOMonad m v where
  readVal :: m v
  writeVal :: v -> m ()

data WStore = WStore
  { valStack :: [Int],
    callStack :: [Int],
    heap :: Map Int Int
  }

runProgram :: (ProgramState m, MonadState WStore m, IOMonad m Int, MonadError String m) => Program WInstruction -> m ()
runProgram program = do
    pc <- getPC
    putPC (pc + 1)
    store <- get
    case program ! pc of
      InputChar -> _
      InputNum -> do
        v <- readVal
        put $ store {valStack = v : valStack store}
        runProgram program
      OutputChar -> do
        case valStack store of
          [] -> throwError "Output on empty stack"
          n : ns -> do
            writeVal n
            put $ store {valStack = ns}
            runProgram program
      OutputNum -> _
      Push n -> _
      Dup -> _
      Swap -> _
      Discard -> _
      Copy n -> _
      Slide n -> _
      Arith wb -> _
      Label n -> _
      Call n -> _
      Branch wc n -> _
      Return -> _
      End -> return ()
      Store -> _
      Retrieve -> _
    return undefined