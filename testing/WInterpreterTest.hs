module WInterpreterTest (qc) where

import Control.Monad.Identity (Identity (..))
import Program (listToArray)
import Test.HUnit (Assertion, Counts, Test (..), assert, runTestTT, (~:), (~?=))
import Test.QuickCheck (Property)
import Test.QuickCheck qualified as QC
import WStepper (MonadReadWrite (..), WError (..), runProgram, runProgramIO)
import WSyntax (WInstruction (..), WBop (..))
import WArbPrograms

-- | ReadWrite plug-ins for testing

instance MonadReadWrite Identity where
  readChar :: Identity Char
  readChar = error "No readChar for identity"
  writeString :: String -> Identity ()
  writeString = error "No writeString for identity"

prop_verifyEmptyStack :: [WInstruction Int] -> Property
prop_verifyEmptyStack instrs = case err of
  Left ValStackEmpty -> QC.collect ValStackEmpty $ not $ stackVerify instrs
  Left _ -> QC.property QC.Discard
  Right () -> QC.collect "success" $ QC.property $ stackVerify instrs
  where
    Identity err = runProgram $ listToArray instrs

prop_validateNonemptyStack :: [WInstruction Int] -> Property
prop_validateNonemptyStack instrs = case err of
  Left ValStackEmpty -> QC.collect ValStackEmpty False
  Left _ -> QC.property QC.Discard
  Right () -> QC.collect "success" $ QC.property True
  where
    Identity err = runProgram $ listToArray instrs

qc :: IO ()
qc = do
  putStrLn "prop_EmptyStackError"
  QC.quickCheck $ QC.forAll (programOf smallStackInstr) prop_verifyEmptyStack
  putStrLn "prop_NonEmptyStackNoError"
  QC.quickCheck $ QC.forAll (stackValidate <$> programOf smallStackInstr) prop_validateNonemptyStack
