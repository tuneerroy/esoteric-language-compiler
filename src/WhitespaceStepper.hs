module WhitespaceStepper where

import Control.Monad.State (State, execState)
import Data.Map (Map)
import Data.Map qualified as Map
import Stepper (Index, PC (..), Steppable (..), mkSteppable)
import WhitespaceParser ()
import WhitespaceSyntax (WInstruction (Label), WVal)

-- | Tests

type WSteppable = Steppable WInstruction

wMkSteppable ::
  (Eq a, Ord a) =>
  [WInstruction a] ->
  Maybe WSteppable
wMkSteppable = mkSteppable extractLabel
  where
    extractLabel (Label label) = Just label
    extractLabel _ = Nothing

data WStore = WStore
  { valStack :: [WVal],
    callStack :: [Index],
    heap :: Map WVal WVal,
    input :: [WVal],
    output :: [WVal]
  }

finalPC :: PC -> Bool
finalPC (PC _) = False
finalPC _ = True

wInitStore :: [WVal] -> WStore
wInitStore input =
  WStore
    { valStack = [],
      callStack = [],
      heap = Map.empty,
      input = input,
      output = []
    }

wStep :: WSteppable -> State WStore WSteppable
wStep steppable = case pc steppable of
  PC n -> undefined
  _ -> return steppable

wEval :: WSteppable -> State WStore WSteppable
wEval steppable
  | finalPC $ pc steppable = return steppable
  | otherwise = wStep steppable >>= wEval

wExec :: WSteppable -> WStore -> WStore
wExec = execState . wEval

wStepper :: IO ()
wStepper = undefined

-- | Tests
-- We'll just use unit tests for this