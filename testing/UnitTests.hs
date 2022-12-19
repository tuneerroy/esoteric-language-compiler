module UnitTests where

import ASyntax (AInstruction, instructionsToStrings)
import BCompiler qualified
import BParser qualified
import BStepper (BStore, execProgram)
import BStepper qualified
import BSyntax (BInstruction)
import Control.Monad (forM_)
import Data.Foldable (sequenceA_)
import Data.Function ((&))
import Data.Functor (void)
import Data.Functor.Identity (Identity (..))
import FakeIO (FakeIO, outputOf)
import JumpProgram (listToArray, mkJumpProgram)
import System.Process (createProcess, getPid, shell, waitForProcess)
import Test.HUnit (Test)
import Test.HUnit.Base (assert)
import WCompiler (compileProgram)
import WParser (WCommand, parseString)
import WStepper (execProgram, execProgramIO)
import WSyntax ()

createUnitTest ::
  String -> -- Directory Name, Ex: "test_files"
  String -> --Filename Extension, Ex: ".ws"
  String -> -- Program Args, Ex: "12+23-*"
  Language a ->
  IO ()
createUnitTest dir ext args lang = do
  putStrLn dir
  let totalPath = dir ++ "/program"
  program <- readFile (totalPath ++ "." ++ ext)
  writeFile (dir ++ "/in.txt") args
  putStrLn "here"
  case parse lang program of
    Nothing -> error "Unable to parse"
    Just parsed -> do
      -- avengers assemble
      writeFile (totalPath ++ ".s") (instructionsToStrings $ compile lang parsed)
      -- compile ARM assembly
      putStrLn "here4"
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
      putStrLn "here5"
      runScript $ "./" ++ totalPath ++ " > " ++ dir ++ "/out.txt"
      actual <- readFile (dir ++ "/out.txt")
      putStrLn "read file"
      case interpret lang parsed args of
        Nothing -> error "Unable to interpret"
        Just expected -> do
          putStr $ "Expected: <>" ++ expected ++ "<>\n"
          putStr $ "Actual: <>" ++ actual ++ "<>\n"
          assert $ expected == actual

debugWS :: String -> String -> IO ()
debugWS dir args = do
  let totalPath = dir ++ "/program"
  programString <- readFile (totalPath ++ ".ws")

  let program = case WParser.parseString programString >>= mkJumpProgram of
        Nothing -> error "Failed to make program"
        Just x -> x

  let res = outputOf (WStepper.execProgram program) args

  print res

  return ()

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

testWS1To100 :: IO ()
testWS1To100 =
  createUnitTest
    (wsFilePath ++ "print1to100")
    "ws"
    ""
    wLanguage

testWSJumpAllTest :: IO ()
testWSJumpAllTest =
  createUnitTest
    (wsFilePath ++ "jumpAll")
    "ws"
    ""
    wLanguage

testWSJumpNegNotTakenTest :: IO ()
testWSJumpNegNotTakenTest =
  createUnitTest
    (wsFilePath ++ "jumpNegNotTaken")
    "ws"
    ""
    wLanguage

testWSJumpZTakenTest :: IO ()
testWSJumpZTakenTest =
  createUnitTest
    (wsFilePath ++ "jumpZTaken")
    "ws"
    ""
    wLanguage

testWSJumpZNotTakenTest :: IO ()
testWSJumpZNotTakenTest =
  createUnitTest
    (wsFilePath ++ "jumpZNotTaken")
    "ws"
    ""
    wLanguage

testBFFactorial :: IO ()
testBFFactorial =
  createUnitTest
    (bfFilePath ++ "factorial")
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

-- | Get the output of a whitespace program on an input string
wInterpreter :: [WCommand] -> String -> Maybe String
wInterpreter commands inputs = do
  instrs <- mkJumpProgram commands
  case outputOf (WStepper.execProgram instrs) inputs of
    Left err -> Nothing
    Right s -> Just s

bLanguage :: Language BInstruction
bLanguage = Language BParser.parseString bInterpreter BCompiler.compileProgram

-- | Get the output of a brainfuck program on an input string
bInterpreter :: [BInstruction] -> String -> Maybe String
bInterpreter instrs input = Just res
  where
    -- We use identity since execProgram is generalized on monads
    Identity res = outputOf (Identity <$> BStepper.execProgram instrs) input

{--

-- testWSFib, (live demo) -- <>
-- testWSCalcTest, -- (live demo) <>
-- testWSNameTest, (live demo) -- <>
-- testBFFactorial, (possibly live?)
-- testBFCat <>

--}

main :: IO ()
main =
  sequenceA_
    [ testWSHelloWorld,
      testWS1To100,
      testWSJumpAllTest,
      testWSJumpNegNotTakenTest,
      testWSJumpZNotTakenTest,
      testWSJumpZTakenTest,
      testBFSquares
    ]