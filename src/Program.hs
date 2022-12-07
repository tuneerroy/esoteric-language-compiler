module Program where

import Control.Monad.State (MonadState (get, put), State)
import Control.Monad.State.Lazy (evalState)
import Data.Map (Map)
import Data.Map qualified as Map
import GHC.Arr (Array, listArray)

-- | An instruction in a program can have a text label
--   that can be replaced by an index at compile time
class Traversable i => Instruction i where
  extractLabel :: i l -> Maybe l

-- | A program is an array of instructions where jump instructions have indices
type Program i = Array Int (i Int)

listToArray :: [a] -> Array Int a
listToArray l = listArray (0, length l - 1) l

-- | Converts a block into a program
mkProgram :: forall i l. (Ord l, Instruction i) => [i l] -> Maybe (Program i)
mkProgram block = listToArray <$> traverse (relabel table) block'
  where
    -- Evaluation of getLabels
    (block', table) = evalState (getLabels block) 0

    -- Removes all label instructions and builds a map from label to index
    getLabels :: [i l] -> State Int ([i l], Map l Int)
    getLabels [] = return ([], Map.empty)
    getLabels (x : xs) = do
      count <- get
      case extractLabel x of
        Nothing -> do
          put (count + 1)
          (block, labelLookup) <- getLabels xs
          return (x : block, labelLookup)
        Just label -> do
          (block, labelLookup) <- getLabels xs
          return (block, Map.insert label count labelLookup)

    -- Given a conversion from string label to index label, perform it
    relabel :: Map l Int -> i l -> Maybe (i Int)
    relabel labelLookup = traverse (`Map.lookup` labelLookup)

type ProgramState s m = MonadState (s, Int) m