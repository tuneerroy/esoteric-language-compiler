module UnitTests where

import ASyntax (AInstruction, instructionsToStrings)
import BCompiler qualified
import BInterpreter (BStore, execProgram)
import BInterpreter qualified
import BParser qualified
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
import WInterpreter (execProgram, execProgramIO)
import WParser (WCommand, parseString)
import WSyntax ()

compileAndRun :: String -> String -> Language a -> IO ()
compileAndRun subdir args l = do
  let fp = dir l ++ subdir
      totalPath = fp ++ "/program"
  program <- readFile (totalPath ++ ext l)
  writeFile (fp ++ "/in.txt") args
  parseAndCheck program fp totalPath args l

parseAndCheck :: String -> String -> String -> String -> Language a -> IO ()
parseAndCheck pgrm fp tp args l = case parse l pgrm of
  Nothing -> error "Unable to parse"
  Just parsed -> do
    -- avengers assemble
    writeFile (fp ++ ".s") (instructionsToStrings $ compile l parsed)
    -- compile ARM assembly
    runScript $ "as -o " ++ tp ++ ".o " ++ tp ++ ".s"
    -- create executable
    runScript $
      "ld -macosx_version_min 11.0.0 -o "
        ++ tp
        ++ " "
        ++ tp
        ++ ".o -lSystem -syslibroot `xcrun -sdk macosx \
           \ --show-sdk-path` -e _start -arch arm64"
    -- run executable and output to "output.txt"
    runScript $ "./" ++ tp ++ " > " ++ fp ++ "/out.txt"
    actual <- readFile (fp ++ "/out.txt")
    putStrLn "read file"
    case interpret l parsed args of
      Nothing -> error "Unable to interpret"
      Just expected -> do
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
wsFilePath = "examples/ws/"

bfFilePath :: String
bfFilePath = "examples/bf/"

testWSHelloWorld :: IO ()
testWSHelloWorld =
  compileAndRun
    "helloworld"
    ""
    wLanguage

testWS1To100 :: IO ()
testWS1To100 =
  compileAndRun
    "print1to100"
    ""
    wLanguage

testWSJumpAllTest :: IO ()
testWSJumpAllTest =
  compileAndRun
    "jumpAll"
    ""
    wLanguage

testWSJumpNegNotTakenTest :: IO ()
testWSJumpNegNotTakenTest =
  compileAndRun
    "jumpNegNotTaken"
    ""
    wLanguage

testWSJumpZTakenTest :: IO ()
testWSJumpZTakenTest =
  compileAndRun
    "jumpZTaken"
    ""
    wLanguage

testWSJumpZNotTakenTest :: IO ()
testWSJumpZNotTakenTest =
  compileAndRun
    "jumpZNotTaken"
    ""
    wLanguage

testBFFactorial :: IO ()
testBFFactorial =
  compileAndRun
    "factorial"
    ""
    bLanguage

testBFSquares :: IO ()
testBFSquares =
  compileAndRun
    "squares"
    ""
    bLanguage

data Language a = Language
  { parse :: String -> Maybe [a],
    interpret :: [a] -> String -> Maybe String,
    compile :: [a] -> [AInstruction],
    dir :: String,
    ext :: String
  }

wLanguage :: Language WCommand
wLanguage =
  Language
    WParser.parseString
    wInterpreter
    WCompiler.compileProgram
    wsFilePath
    ".ws"

-- | Get the output of a whitespace program on an input string
wInterpreter :: [WCommand] -> String -> Maybe String
wInterpreter commands inputs = do
  instrs <- mkJumpProgram commands
  case outputOf (WInterpreter.execProgram instrs) inputs of
    Left err -> Nothing
    Right s -> Just s

bLanguage :: Language BInstruction
bLanguage =
  Language
    BParser.parseString
    bInterpreter
    BCompiler.compileProgram
    bfFilePath
    ".b"

-- | Get the output of a brainfuck program on an input string
bInterpreter :: [BInstruction] -> String -> Maybe String
bInterpreter instrs input = Just res
  where
    -- We use identity since execProgram is generalized on monads
    Identity res = outputOf (Identity <$> BInterpreter.execProgram instrs) input

main :: IO ()
main =
  sequenceA_
    [ testWSHelloWorld,
      testWS1To100,
      testWSJumpAllTest,
      testWSJumpNegNotTakenTest,
      testWSJumpZNotTakenTest,
      testWSJumpZTakenTest
      -- testBFSquares
    ]