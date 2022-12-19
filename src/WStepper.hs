{-# LANGUAGE TemplateHaskell #-}

module WStepper where

import Control.Lens (Ixed (ix), makeLenses, (%~), (&), (.~), (^.), (^?))
import Control.Monad (forM_, unless, void, when)
import Control.Monad.Except (ExceptT, MonadError (..), runExceptT)
import Control.Monad.State
  ( MonadState (get, put),
    StateT (runStateT),
    evalStateT,
    execStateT,
  )
import Control.Monad.State.Lazy (StateT, gets, modify)
import Control.Monad.Trans (MonadTrans (..))
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe (fromMaybe)
import GHC.Arr (Ix (range), (!))
import MonadReadWrite (MonadReadWrite (..), readLine)
import Program (Program, ProgramState, listToArray)
import WSyntax (WBop (..), WCond (..), WInstruction (..))

data WStore = WStore
  { _valStack :: [Int],
    _callStack :: [Int],
    _heap :: Map Int Int
  }
  deriving (Show)

makeLenses ''WStore

initStore :: WStore
initStore = WStore [] [] Map.empty

initState :: (WStore, Int)
initState = (initStore, 0)

data WError
  = ProgramOutOfBounds Int
  | InvalidOutputChar
  | NegativeHeapKey
  | ValStackEmpty
  | CallStackEmpty
  | LabelFound
  | DivideByZero
  deriving (Eq, Show)

toProgramState :: forall m. (ProgramState WStore m, MonadReadWrite m, MonadError WError m) => Program WInstruction -> m ()
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
                Div -> div
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
    (store, pc) <- get
    put (store, pc + 1)
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
      if addr < 0
        then throwError NegativeHeapKey
        else put (store & heap %~ Map.insert addr val, pc)

execProgram :: MonadReadWrite m => Program WInstruction -> m (Either WError (WStore, Int))
execProgram program = do
  let programState = toProgramState program
  runExceptT $ execStateT programState initState

execProgramIO :: Program WInstruction -> IO (WStore, Int)
execProgramIO program = do
  possibleError <- execProgram program
  case possibleError of
    Left err -> error $ show err
    Right state -> return state

arr = listToArray [1, 2, 3, 4]

-- >>> arr ^? ix 0
-- Just 1
