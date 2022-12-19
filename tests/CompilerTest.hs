module CompilerTest (qc) where

import ASyntax (toArm64String)
import Control.Monad (unless, when)
import Control.Monad.Reader (MonadReader (ask), MonadTrans (lift), ReaderT (runReaderT))
import Data.Function ((&))
import Data.List (intercalate)
import Data.Maybe (isJust)
import FakeIO (outputOf)
import JumpProgram (listToArray, mkJumpProgram)
import System.Process (CreateProcess, createProcess, shell, waitForProcess)
import Test.HUnit (Test, assert, runTestTT, (~:))
import Test.QuickCheck (Arbitrary (..), Property)
import Test.QuickCheck qualified as QC
import Test.QuickCheck.Gen (Gen)
import Test.QuickCheck.Monadic qualified as QC
import Test.QuickCheck.Property qualified as QC
import WArbPrograms (checkProp, validHeapAndOutputProgram, validOutputProgram)
import WCompiler (compileProgram)
import WInterpreter (WError (ValStackEmpty), execProgram, initState)
import WParser (WCommand)
import WParserTest qualified
import WSyntax (WInstruction (..))

progFile :: FilePath
progFile = "examples/qcoutput/prog.s"

script :: CreateProcess
script = shell "examples/qcoutput/script.sh"

outFile :: FilePath
outFile = "examples/qcoutput/out.txt"

-- | Quickcheck
prop_model :: [WCommand] -> Property
prop_model commands = QC.monadicIO $ do
  --Get the interpreted output
  let maybeInterpretedOutput = do
        arr <- mkJumpProgram commands
        case outputOf (execProgram arr) [] of
          Left _ -> Nothing
          Right s -> return s

  -- If no output is interpreted, discard the test
  case maybeInterpretedOutput of
    Nothing -> QC.stop QC.rejected
    Just interpretedOutput -> do
      -- Write the assembly file
      compileProgram commands
        & map toArm64String
        & intercalate "\n"
        & writeFile progFile
        & QC.run

      -- Run the shell script
      (_, _, _, handle) <- QC.run $ createProcess script
      QC.run $ waitForProcess handle

      -- Read in the output
      executableOutput <- QC.run $ readFile outFile

      let condition = executableOutput == interpretedOutput

      QC.assert (executableOutput == interpretedOutput)

qc :: IO ()
qc = do
  putStrLn "prop_model no heap"
  checkProp validOutputProgram prop_model
  putStrLn "prop_model with heap"
  checkProp validHeapAndOutputProgram prop_model