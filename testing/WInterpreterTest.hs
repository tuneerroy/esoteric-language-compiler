module WInterpreterTest (qc) where

import Data.Function
import FakeIO (outputOf)
import Program (listToArray)
import Test.HUnit (Assertion, Counts, Test (..), assert, runTestTT, (~:), (~?=))
import Test.QuickCheck (Property)
import Test.QuickCheck qualified as QC
import WArbPrograms
import WStepper (MonadReadWrite (..), WError (..), execProgram)
import WSyntax (WBop (..), WInstruction (..))

prop_verifyEmptyStack :: [WInstruction Int] -> Property
prop_verifyEmptyStack instrs = case err of
  Left ValStackEmpty -> instrs & QC.collect ValStackEmpty . not . stackVerify
  Left _ -> QC.property QC.Discard
  Right _ -> instrs & QC.collect "success" . QC.property . stackVerify
  where
    err = outputOf (execProgram $ listToArray instrs) []

prop_validateNonemptyStack :: [WInstruction Int] -> Property
prop_validateNonemptyStack instrs = case err of
  Left ValStackEmpty -> QC.collect ValStackEmpty False
  Left _ -> QC.property QC.Discard
  Right _ -> QC.collect "success" $ QC.property True
  where
    err = outputOf (execProgram $ listToArray instrs) []

prop_outputCount :: [WInstruction Int] -> Property
prop_outputCount instrs = case err of
  Left we -> QC.property QC.Discard
  Right s -> QC.property (length s >= outputCount instrs)
  where
    err = outputOf (execProgram $ listToArray instrs) []
    outputCount = length . filter (\x -> x `elem` [OutputChar, OutputNum])

qc :: IO ()
qc = do
  putStrLn "prop_EmptyStackError"
  QC.quickCheck $ QC.forAll stackProgram prop_verifyEmptyStack
  putStrLn "prop_NonEmptyStackNoError"
  QC.quickCheck $ QC.forAll validStackProgram prop_validateNonemptyStack
  putStrLn "prop_outputCount"
  QC.quickCheck $ QC.forAll validOutputProgram prop_outputCount