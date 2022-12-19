module CompilerTest (qc) where

import Control.Monad.Reader (MonadReader (ask), MonadTrans (lift), ReaderT (runReaderT))
import Control.Monad.State (State, StateT (..), evalState)
import Control.Monad.State.Lazy (MonadState (..))
import Data.Map qualified as Map
import Program (listToArray, mkProgram)
import System.Process (CreateProcess, createProcess, shell)
import Test.HUnit (Test, assert, runTestTT, (~:))
import Test.QuickCheck (Arbitrary (..), Property)
import Test.QuickCheck qualified as QC
import Test.QuickCheck.Gen (Gen)
import Test.QuickCheck.Monadic (monadicIO, run)
import Test.QuickCheck.Monadic qualified as QC
import WStepper (MonadReadWrite (readChar, writeString), WError (ValStackEmpty), initState, runProgram, runProgramIO)
import WSyntax (WInstruction (..))
import Control.Monad.Identity (Identity)
import Control.Lens (Identity(..))
import WParser (WCommand)
import WCompiler (compileProgram)
import ASyntax (toArm64String)
import Data.List (intercalate)
import Data.Function ((&))

outFile :: FilePath
outFile = "/whitespace/quickCheckTest.asm"

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

newtype FileO a = FileO (ReaderT FilePath IO a)
  deriving (Functor, Applicative, Monad)

instance MonadReadWrite FileO where
  readChar :: FileO Char
  readChar = error "No read instance for FileO"
  writeString :: String -> FileO ()
  writeString s = FileO $ do
    filePath <- ask
    lift $ appendFile filePath s

-- | Quickcheck
prop_model :: [WCommand] -> Property
prop_model commands = monadicIO $ do

  -- Write the assembly file
  compileProgram commands
    & map toArm64String
    & intercalate "\n"
    & writeFile outFile
    & run

  

  -- let assemblyStr = map toArm64String assembly
  -- return (intercalate "\n" assemblyStr)

  QC.assert True

qc :: IO ()
qc = do return ()
