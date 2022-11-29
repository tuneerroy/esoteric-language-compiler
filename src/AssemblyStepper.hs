module AssemblyStepper where

import Control.Monad.State (State, execState)
import Data.Map (Map)
import Data.Map qualified as Map
import Stepper (Index, PC (..), Steppable (..), mkSteppable)
import AssemblySyntax (AInstruction (Label))

-- | Tests
type WSteppable = Steppable AInstruction

wMkSteppable ::
  (Eq a, Ord a) =>
  [AInstruction a] ->
  Maybe WSteppable
wMkSteppable = mkSteppable extractLabel
  where
    extractLabel (Label label) = Just label
    extractLabel _ = Nothing

-- | Do we need this??
