-- | A module for dealing with programming languages that use jumps as their flow control
module JumpProgram where

import Control.Monad.State (MonadState (get, put), State)
import Control.Monad.State.Lazy (evalState)
import Data.Map (Map)
import Data.Map qualified as Map
import GHC.Arr (Array, listArray, (!))
import Test.HUnit (Test (..), (~:), (~?=))
import WSyntax (WBop (..), WCond (..), WInstruction (..))

-- | An instruction in such a program will have text labels that
-- | can be replaced by indicues at compile times
class Traversable i => JumpProgramInstruction i where
  extractLabel :: i l -> Maybe l

-- | A program is an array of instructions where jump instructions have indices
type JumpProgram i = Array Int (i Int)

listToArray :: [a] -> Array Int a
listToArray l = listArray (0, length l - 1) l

-- | Converts a block into a program.
-- | Labels are removed, and all references to them are replaced by the appropriate index
mkJumpProgram :: forall i l. (Ord l, JumpProgramInstruction i) => [i l] -> Maybe (JumpProgram i)
mkJumpProgram block = listToArray <$> traverse (relabel table) block'
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

-- type ProgramState s m = MonadState (s, Int) m

instance JumpProgramInstruction WInstruction where
  extractLabel :: WInstruction l -> Maybe l
  extractLabel (Label l) = Just l
  extractLabel _ = Nothing

-- Testing sample programs
sampleProgramNoLabels :: [WInstruction String]
sampleProgramNoLabels = [Push 5, Dup, Arith Add, End]

sampleProgramWithLabels :: [WInstruction String]
sampleProgramWithLabels = [Label " \t\t\n"]

mkProgramTest :: Test
mkProgramTest =
  "mkProgram tests"
    ~: TestList
      [ mkJumpProgram sampleProgramNoLabels ~?= Just (listToArray [Push 5, Dup, Arith Add, End]),
        mkJumpProgram sampleProgramWithLabels ~?= Just (listToArray [])
      ]