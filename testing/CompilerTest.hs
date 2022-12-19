module CompilerTest (qc) where

import ASyntax (toArm64String)
import Control.Monad.Reader (MonadReader (ask), MonadTrans (lift), ReaderT (runReaderT))
import Data.Function ((&))
import Data.List (intercalate)
import Data.Maybe (isJust)
import FakeIO (outputOf)
import GHC.TopHandler (flushStdHandles)
import Program (listToArray, mkProgram)
import System.Process (CreateProcess, createProcess, shell, waitForProcess)
import Test.HUnit (Test, assert, runTestTT, (~:))
import Test.QuickCheck (Arbitrary (..), Property)
import Test.QuickCheck qualified as QC
import Test.QuickCheck.Gen (Gen)
import Test.QuickCheck.Monadic qualified as QC
import Test.QuickCheck.Property qualified as QC
import WArbPrograms (validHeapAndOutputProgram, validOutputProgram)
import WCompiler (compileProgram)
import WParser (WCommand)
import WParserTest qualified
import WStepper (WError (ValStackEmpty), execProgram, initState)
import WSyntax (WInstruction (..))

progFile :: FilePath
progFile = "test_files/qcoutput/prog.s"

script :: CreateProcess
script = shell "test_files/qcoutput/script.sh"

outFile :: FilePath
outFile = "test_files/qcoutput/out.txt"

s :: CreateProcess
s = shell "bash script.sh"

-- >>> createProcess s

-- | Unit tests
tHelloWorld :: Test
tHelloWorld =
  "hello world" ~: test2

test :: IO ()
test = do
  putStrLn "hello world"
  _ <- createProcess s
  program <- readFile "out.txt"
  putStrLn "aa"

test2 :: IO ()
test2 = do
  _ <- createProcess s
  str <- readFile "out.txt"
  assert (str == "Hola, mundo\n")

temp :: IO String
temp = readFile "out.txt"

-- >>> temp
-- "Hola, mundo\n"

-- | Quickcheck
prop_model :: [WCommand] -> Property
prop_model commands = QC.monadicIO $ do
  --Get the interpreted output
  let maybeInterpretedOutput = do
        arr <- mkProgram commands
        case outputOf (execProgram arr) [] of
          Left _ -> Nothing
          Right s -> return s

  -- If no output is interpreted, fail entirely
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

      QC.run $ print executableOutput

      QC.assert (filter badChar executableOutput == filter badChar interpretedOutput)
  where
    badChar :: Char -> Bool
    badChar c =
      let asciiCode = fromEnum c
       in asciiCode >= 32 && asciiCode <= 255

qc :: IO ()
qc = do
  putStrLn "Prop Model"
  QC.quickCheck $ QC.forAll validHeapAndOutputProgram prop_model