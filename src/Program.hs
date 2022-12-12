module Program where

import Control.Monad.State (MonadState (get, put), State)
import Control.Monad.State.Lazy (evalState)
import Data.Map (Map)
import Data.Map qualified as Map
import GHC.Arr (Array, listArray)
import Test.HUnit (Test (..), (~:), (~?=))
import WParser (WLabel)
import WSyntax (WBop (..), WInstruction (..))

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

-- TESTS
instance Instruction WInstruction where
  extractLabel :: WInstruction l -> Maybe l
  extractLabel (Label l) = Just l
  extractLabel _ = Nothing

sampleProgramNoLabels :: [WInstruction String]
sampleProgramNoLabels = [Push 5, Dup, Arith Add, End]

sampleProgramWithLabels :: [WInstruction String]
sampleProgramWithLabels = [Label " \t\t\n"]

mkProgramTest :: Test
mkProgramTest =
  "mkProgram tests"
    ~: TestList
      [ mkProgram sampleProgramNoLabels ~?= Just (listToArray [Push 5, Dup, Arith Add, End]),
        mkProgram sampleProgramWithLabels ~?= Just (listToArray [Label 0])
      ]

-- if a WInstruction is in the block, it will be in the Array as well
-- prop_program_contains_ins