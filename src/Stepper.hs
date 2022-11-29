module Stepper where

import Control.Monad.State (StateT)
import Data.Map (Map)
import Data.Map qualified as Map
import GHC.Arr (Array, listArray)

-- | An index into the program; not sure if this should be Int or Integer
type Index = Int

-- | A data type for a pogram counter
data PC = PC Index | Error String | End

-- | By using an array, we can have efficient access to the program
data Steppable i = Steppable {
    pc :: PC,
    program :: Array Int (i Int)
}

-- | Create a steppable out of a list of instructions
mkSteppable :: forall i t. (Eq t, Ord t, Functor i, Traversable i) =>
    (i t -> Maybe t) -> [i t] -> Maybe (Steppable i)
mkSteppable extractLabel block = do
    labeledBlock <- traverse extract block
    return Steppable {pc = PC 0, program = listArray (0, length labeledBlock - 1) labeledBlock}
  where
    getLabels = go 0 Map.empty
      where
        go i table [] = table
        go i table (x : xs) = case extractLabel x of
          Just label -> go i (Map.insert label i table) xs
          Nothing -> go (i + 1) table xs
    extract = traverse (\k -> Map.lookup k (getLabels block))

