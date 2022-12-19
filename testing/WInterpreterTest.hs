module WInterpreterTest (qc) where

import Control.Applicative (liftA3)
import Control.Lens ((&), (^.))
import Data.Function ((&))
import Data.Maybe (isJust, isNothing)
import FakeIO (finalStateOf, outputOf)
import JumpProgram (listToArray)
import Test.HUnit (Assertion, Counts, Test (..), assert, runTestTT, (~:), (~?=))
import Test.QuickCheck (Property)
import Test.QuickCheck qualified as QC
import WArbPrograms
  ( stackProgram,
    stackVerify,
    validHeapAndOutputProgram,
    validOutputProgram,
    validStackProgram,
  )
import WStepper (WError (..), execProgram, valStack)
import WSyntax (WBop (..), WInstruction (..))

-- | Checks that programs that should overflow the stack actually do
prop_verifyEmptyStack :: [WInstruction Int] -> Property
prop_verifyEmptyStack instrs = case err of
  Left ValStackEmpty -> instrs & QC.collect ValStackEmpty . isNothing . stackVerify
  Left _ -> QC.property QC.Discard
  Right (state, _) -> QC.property (Just (state ^. valStack & length) == stackVerify instrs)
  where
    err = finalStateOf (execProgram $ listToArray instrs) []

-- Checks that validated programs don't overflow thes stack
prop_validateNonemptyStack :: [WInstruction Int] -> Property
prop_validateNonemptyStack instrs = case err of
  Left ValStackEmpty -> QC.collect ValStackEmpty False
  Left _ -> QC.property QC.Discard
  Right (state, _) -> QC.property (Just (state ^. valStack & length) == stackVerify instrs)
  where
    err = finalStateOf (execProgram $ listToArray instrs) []

-- | Checks that storing a value at the beginning of the program then
-- | retreiving it at the end is valid
prop_heap :: (Int, Int, [WInstruction Int]) -> Property
prop_heap (val, addr, instrs) = case finalState of
  Left _ -> QC.property QC.Discard
  Right (state, _) -> case state ^. valStack of
    n : _ -> QC.property (n == val)
    _ -> QC.property False
  where
    addRetrieve [] = error ""
    addRetrieve [End] = [Push addr, Retrieve, End]
    addRetrieve (x : xs) = x : addRetrieve xs

    addHeap instrs =
      Push addr : Push val : Store : addRetrieve instrs

    finalState = finalStateOf (execProgram $ listToArray (addHeap instrs)) []

-- | Checks that output commands actually output values
prop_outputCount :: [WInstruction Int] -> Property
prop_outputCount instrs = case err of
  Left we -> QC.property QC.Discard
  Right s -> QC.property (length s >= outputCount instrs)
  where
    err = outputOf (execProgram $ listToArray instrs) []
    outputCount = length . filter (\x -> x `elem` [OutputChar, OutputNum])

qc :: IO ()
qc = do
  putStrLn "prop_verifyEmptyStack"
  QC.quickCheck $ QC.forAll stackProgram prop_verifyEmptyStack
  putStrLn "prop_validateNonemptyStack"
  QC.quickCheck $ QC.forAll validStackProgram prop_validateNonemptyStack
  putStrLn "prop_validateNonemptyStack with heap"
  QC.quickCheck $ QC.forAll validStackProgram prop_validateNonemptyStack
  putStrLn "prop_outputCount"
  QC.quickCheck $ QC.forAll validOutputProgram prop_outputCount
  putStrLn "prop_heap"
  QC.quickCheck $ QC.forAll (liftA3 (,,) QC.arbitrary QC.arbitrary validStackProgram) prop_heap