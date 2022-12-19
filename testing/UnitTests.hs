module UnitTests where

import ASyntax (AInstruction, instructionsToStrings)
import BCompiler qualified
import BParser qualified
import BStepper qualified
import BSyntax (BInstruction)
import Control.Monad (forM_)
import Data.Foldable (sequenceA_)
import Data.Function ((&))
import FakeIO (outputOf)
import Program (listToArray, mkProgram)
import System.Process (createProcess, shell, waitForProcess)
import Test.HUnit (Test)
import Test.HUnit.Base (assert)
import WCompiler (compileProgram)
import WParser (WCommand, wParseString)
import WStepper (execProgram)
import WSyntax ()

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

testTruthMachine :: IO ()
testTruthMachine =
  createUnitTest
    (wsFilePath ++ "truthmachine")
    "ws"
    ""
    wLanguage

runScript ::
  String -> IO ()
runScript s = do
  (_, _, _, handle) <- s & shell & createProcess
  waitForProcess handle
  return ()

data Language a = Language
  { parse :: String -> Maybe [a],
    interpret :: [a] -> String -> Maybe String,
    compile :: [a] -> [AInstruction]
  }

wLanguage :: Language WCommand
wLanguage = Language WParser.wParseString wInterpreter WCompiler.compileProgram

wInterpreter :: [WCommand] -> String -> Maybe String
wInterpreter commands inputs = do
  instrs <- mkProgram commands
  case outputOf (execProgram instrs) inputs of
    Left err -> Nothing
    Right s -> Just s

bLanguage :: Language BInstruction
bLanguage = Language BParser.bParseString bInterpreter BCompiler.compileProgram

bInterpreter :: [BInstruction] -> String -> Maybe String
bInterpreter = undefined

main :: IO ()
main =
  sequenceA_
    [ testWSHelloWorld,
      testWSAsterikGrid,
      testWSCat,
      testWSDivDoub,
      testWS1To100,
      testTruthMachine
    ]