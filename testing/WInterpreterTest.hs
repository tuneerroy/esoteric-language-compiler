module WInterpreterTest (qc) where

import Control.Monad.Identity (Identity (..))
import Program (listToArray)
import Test.HUnit (Assertion, Counts, Test (..), assert, runTestTT, (~:), (~?=))
import Test.QuickCheck (Property)
import Test.QuickCheck qualified as QC
import WStepper (MonadReadWrite (..), WError (..), runProgram, runProgramIO)
import WSyntax (WInstruction (..), WBop (..))
import WArbPrograms ( NEStackProg(..), stackVerify, StackProg(..) )

-- | ReadWrite plug-ins for testing

instance MonadReadWrite Identity where
  readChar :: Identity Char
  readChar = error "No readChar for identity"
  writeString :: String -> Identity ()
  writeString = error "No writeString for identity"

prop_emptyStackError :: StackProg -> Property
prop_emptyStackError (StackProg instrs) = case err of
  Left ValStackEmpty -> QC.collect ValStackEmpty $ not $ stackVerify instrs
  Left _ -> QC.property QC.Discard
  Right () -> QC.collect "success" $ QC.property $ stackVerify instrs
  where
    Identity err = runProgram $ listToArray instrs

prop_nonemptyStackNoError :: NEStackProg -> Property
prop_nonemptyStackNoError program = case err of
  Left ValStackEmpty -> QC.collect ValStackEmpty False
  Left _ -> QC.property QC.Discard
  Right () -> QC.collect "success" $ QC.property True
  where
    NEStackProg instrs = program
    Identity err = runProgram $ listToArray instrs



qc :: IO ()
qc = do
  putStrLn "prop_EmptyStackError"
  QC.quickCheck prop_emptyStackError
  putStrLn "prop_NonEmptyStackNoError"
  QC.quickCheck prop_nonemptyStackNoError
