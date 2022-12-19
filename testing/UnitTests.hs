module UnitTests where

import ASyntax (AInstruction, instructionsToStrings)
import BCompiler qualified
import BParser qualified
import BStepper (execProgram, BStore)
import BStepper qualified
import BSyntax (BInstruction)
import Control.Monad (forM_)
import Data.Foldable (sequenceA_)
import Data.Function ((&))
import Data.Functor (void)
import FakeIO (outputOf, FakeIO)
import Program (listToArray, mkProgram)
import System.Process (createProcess, getPid, shell, waitForProcess)
import Test.HUnit (Test)
import Test.HUnit.Base (assert)
import WCompiler (compileProgram)
import WParser (WCommand, parseString)
import WStepper (execProgram)
import WSyntax ()
import Data.Functor.Identity (Identity(..))

-- take in filename (including directory)
-- read in the data, hold in string
-- take in input...? (and maybe expected output?)
-- run it on interpreter, store result
-- run it on respective compiler
-- convert to asm
-- paste asm into file
-- run shell script on file (pass in input.txt and output.txt)
-- read in resulting data (from output.txt)
--

-- Directory -> Filename -> Program Args -> Parser
createUnitTest ::
  String -> -- Directory Name, Ex: "test_files"
  String -> --Filename Extension, Ex: ".ws"
  String -> -- Program Args, Ex: "12+23-*"
  Language a ->
  IO ()
createUnitTest dir ext args lang = do
  let totalPath = dir ++ "/program"
  program <- readFile (totalPath ++ "." ++ ext)
  writeFile (dir ++ "/in.txt") args
  case parse lang program of
    Nothing -> error "Invalid program"
    Just parsed ->
      case interpret lang parsed args of
        Nothing -> error "Unable to parse"
        Just expected -> do
          -- avengers assemble
          writeFile (totalPath ++ ".s") (instructionsToStrings $ compile lang parsed)
          -- compile ARM assembly
          runScript $ "as -o " ++ totalPath ++ ".o " ++ totalPath ++ ".s"
          -- create executable
          runScript $
            "ld -macosx_version_min 11.0.0 -o "
              ++ totalPath
              ++ " "
              ++ totalPath
              ++ ".o -lSystem -syslibroot `xcrun -sdk macosx \
                 \ --show-sdk-path` -e _start -arch arm64"
          -- run executable and output to "output.txt"
          runScript $ "./" ++ totalPath ++ " > " ++ dir ++ "/out.txt"
          actual <- readFile (dir ++ "/out.txt")
          putStr $ "Expected: <>" ++ expected ++ "<>\n"
          putStr $ "Actual: <>" ++ actual ++ "<>\n"
          assert $ expected == actual

runScript ::
  String -> IO ()
runScript s = do
  (_, _, _, handle) <- s & shell & createProcess
  waitForProcess handle
  pid <- getPid handle
  case pid of
    Nothing -> return ()
    Just cp -> do
      (_, _, _, handle') <- "kill -INT " ++ show cp & shell & createProcess
      void $ waitForProcess handle'
  return ()

wsFilePath :: String
wsFilePath = "test_files/ws/"

bfFilePath :: String
bfFilePath = "test_files/bf/"

testWSHelloWorld :: IO ()
testWSHelloWorld =
  createUnitTest
    (wsFilePath ++ "helloworld")
    "ws"
    ""
    wLanguage

testWSAsterikGrid :: IO ()
testWSAsterikGrid =
  createUnitTest
    (wsFilePath ++ "asterikgrid")
    "ws"
    ""
    wLanguage

testWSCat :: IO ()
testWSCat =
  createUnitTest
    (wsFilePath ++ "cat")
    "ws"
    "gema kgk aem134 523fekma"
    wLanguage

testWSDivDoub :: IO ()
testWSDivDoub =
  createUnitTest
    (wsFilePath ++ "divdoub")
    "ws"
    ""
    wLanguage

testWS1To100 :: IO ()
testWS1To100 =
  createUnitTest
    (wsFilePath ++ "print1to100")
    "ws"
    ""
    wLanguage

testWSTruthMachine :: IO ()
testWSTruthMachine =
  createUnitTest
    (wsFilePath ++ "truthmachine")
    "ws"
    ""
    wLanguage

testBFBSort :: IO ()
testBFBSort =
  createUnitTest
    (bfFilePath ++ "bsort")
    "b"
    ""
    bLanguage

testBFFactorial :: IO ()
testBFFactorial =
  createUnitTest
    (bfFilePath ++ "factorial")
    "b"
    ""
    bLanguage

testBFHead :: IO ()
testBFHead =
  createUnitTest
    (bfFilePath ++ "head")
    "b"
    ""
    bLanguage

testBFIsort :: IO ()
testBFIsort =
  createUnitTest
    (bfFilePath ++ "isort")
    "b"
    ""
    bLanguage

testBFQsort :: IO ()
testBFQsort =
  createUnitTest
    (bfFilePath ++ "qsort")
    "b"
    ""
    bLanguage

testBFSquares :: IO ()
testBFSquares =
  createUnitTest
    (bfFilePath ++ "squares")
    "b"
    ""
    bLanguage

data Language a = Language
  { parse :: String -> Maybe [a],
    interpret :: [a] -> String -> Maybe String,
    compile :: [a] -> [AInstruction]
  }

wLanguage :: Language WCommand
wLanguage = Language WParser.parseString wInterpreter WCompiler.compileProgram

wInterpreter :: [WCommand] -> String -> Maybe String
wInterpreter commands inputs = do
  instrs <- mkProgram commands
  case outputOf (WStepper.execProgram instrs) inputs of
    Left err -> Nothing
    Right s -> Just s

bLanguage :: Language BInstruction
bLanguage = Language BParser.parseString bInterpreter BCompiler.compileProgram

bInterpreter :: [BInstruction] -> String -> Maybe String
bInterpreter instrs inputs = undefined where
  x :: FakeIO BStore = BStepper.execProgram instrs
  --let Identity s = outputOf (Identity <$> BStepper.execProgram instrs) inputs in Just s

main :: IO ()
main =
  sequenceA_
    [ testWSHelloWorld,
      testWSAsterikGrid,
      testWSCat,
      testWSDivDoub,
      testWS1To100,
      testWSTruthMachine
    ]