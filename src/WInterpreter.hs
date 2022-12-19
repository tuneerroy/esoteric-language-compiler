{-# LANGUAGE TemplateHaskell #-}

module WInterpreter where

import Control.Lens (Ixed (ix), makeLenses, (%~), (&), (.~), (^.), (^?))
import Control.Monad (forM_, unless, void, when)
import Control.Monad.Except (ExceptT, MonadError (..), runExceptT)
import Control.Monad.State
  ( MonadState (get, put),
    execStateT,
  )
import Control.Monad.State.Lazy (StateT, gets)
import Control.Monad.Trans (MonadTrans (..))
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe (fromMaybe)
import GHC.Arr (Ix (range))
import JumpProgram (JumpProgram)
import MonadReadWrite (MonadReadWrite (..), readLine)
import WSyntax (WBop (..), WCond (..), WInstruction (..))

-- | A representation of the internal store of a whitespace program
data WStore = WStore
  { _valStack :: [Int],
    _callStack :: [Int],
    _heap :: Map Int Int
  }
  deriving (Show)

makeLenses ''WStore

-- | The inittial store of a whitespace program
initStore :: WStore
initStore = WStore [] [] Map.empty

-- | The initial state of a whitespace program. The 0 represents the program counter
initState :: (WStore, Int)
initState = (initStore, 0)

-- | Possible errors that the whitesspace program can throw
data WError
  = ProgramOutOfBounds Int
  | InvalidOutputChar
  | NegativeHeapKey
  | ValStackEmpty
  | CallStackEmpty
  | LabelFound
  | DivideByZero
  deriving (Eq, Show)

-- | A monad, that when run, executes the program
toProgramState ::
  forall m.
  (MonadState (WStore, Int) m, MonadReadWrite m, MonadError WError m) =>
  JumpProgram WInstruction ->
  m ()
toProgramState program = do
  (store, pc) <- get
  -- writeString $ show $ store ^. valStack
  instr <- case program ^? ix pc of
    Nothing -> throwError (ProgramOutOfBounds pc)
    Just wi -> return wi
  case instr of
    InputChar -> readChar >>= heapPlace . fromEnum
    InputNum -> readLine >>= heapPlace . read
    OutputChar -> do
      n <- pop
      if (n /= 10) && (n < 32 || n > 126)
        then throwError InvalidOutputChar
        else writeString [toEnum n]
    OutputNum -> do
      n <- pop
      writeString $ show n
    Push n -> push n
    Dup -> do
      n <- pop
      push n
      push n
    Swap -> do
      n <- pop
      m <- pop
      push n
      push m
    Discard -> void pop
    Copy i -> do
      n <- case store ^. valStack ^? ix (fromEnum i) of
        Nothing -> throwError ValStackEmpty
        Just v -> pure v
      push n
    Slide n -> do
      top <- pop
      forM_ [1 .. fromEnum n] $ const pop
      push top
    Arith wb -> do
      b <- pop
      a <- pop
      if wb `elem` [Div, Mod] && b == 0
        then throwError DivideByZero
        else do
          let op = case wb of
                Add -> (+)
                Sub -> (-)
                Mul -> (*)
                Div -> quot
                Mod -> rem
          push $ op a b
    Label n -> throwError LabelFound
    Call n -> put (store & callStack %~ (pc :), n - 1)
    Jump n -> put (store, n - 1)
    Branch wc n -> do
      top <- pop
      store' <- gets fst
      -- writeString ("Branch: " ++ show top)
      let jump = case wc of
            Zero -> top == 0
            Neg -> top < 0
      when jump $ put (store', n - 1)
    Return -> do
      case store ^. callStack of
        [] -> throwError CallStackEmpty
        n : ns -> put (store & callStack .~ ns, n)
    End -> return ()
    Store -> pop >>= heapPlace
    Retrieve -> do
      addr <- pop
      if addr < 0
        then throwError NegativeHeapKey
        else push $ fromMaybe 0 (store ^. heap & Map.lookup addr)
  unless (instr == End) $ do
    (store', pc') <- get
    put (store', pc' + 1)
    toProgramState program
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

    heapPlace :: Int -> m ()
    heapPlace val = do
      (store, pc) <- get
      addr <- pop
      (store', _) <- get
      if addr < 0
        then throwError NegativeHeapKey
        else put (store' & heap %~ Map.insert addr val, pc)

-- | Executes the a whitespace program, returning either an error or the program state
execProgram :: MonadReadWrite m => JumpProgram WInstruction -> m (Either WError (WStore, Int))
execProgram program = do
  let programState = toProgramState program
  runExceptT $ execStateT programState initState

-- | Runs the program using standard in and out for input and output.
execProgramIO :: JumpProgram WInstruction -> IO (WStore, Int)
execProgramIO program = do
  possibleError <- execProgram program
  case possibleError of
    Left err -> error $ show err
    Right state -> return state