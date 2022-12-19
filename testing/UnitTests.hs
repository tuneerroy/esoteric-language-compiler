module UnitTests where

import ASyntax
import BCompiler qualified
import BParser qualified
import BStepper qualified
import BSyntax (BInstruction)
import Data.Function
import FakeIO (outputOf)
import Program (listToArray, mkProgram)
import System.Process
import Test.HUnit (Test)
import Test.HUnit.Base (assert)
import WCompiler
import WParser
import WStepper (runProgram)
import WSyntax

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
main ::
  String -> -- Directory Name, Ex: "test_files"
  String -> --Filename Extension, Ex: ".ws"
  String -> -- Program Args, Ex: "12+23-*"
  Language a ->
  IO ()
main dir ext args lang = do
  let totalPath = dir ++ "/program"
  program <- readFile (totalPath ++ "." ++ ext)
  createInputFile dir args
  case parse lang program of
    Nothing -> error "Invalid program"
    Just parsed ->
      case interpret lang parsed args of
        Nothing -> error "Unable to parse"
        Just expected -> do
          -- avengers assemble
          runScript $ "as -o " ++ totalPath ++ ".o " ++ totalPath ++ ".s"
          -- compile ARM assembly
          runScript $
            "ld -macosx_version_min 11.0.0 -o "
              ++ totalPath
              ++ " "
              ++ totalPath
              ++ ".o -lSystem -syslibroot `xcrun -sdk macosx \
                 \ --show-sdk-path` -e _start -arch arm64"
          runScript $ ".\\" ++ totalPath
          -- run executable and output to "output.txt"
          runScript $ "./" ++ totalPath ++ " > " ++ dir ++ "/out.txt"
          actual <- readOutputFile dir
          assert $ expected == actual

test :: IO ()
test = main "test_files/ws/helloworld" "ws" "" wLanguage

runScript ::
  String -> IO ()
runScript s = do
  (_, _, _, handle) <- s & shell & createProcess
  waitForProcess handle
  return ()

runInterpreter :: [a] -> String
runInterpreter = error "not implemented yet"

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
  case outputOf (runProgram instrs) inputs of
    Left err -> Nothing
    Right s -> Just s

bLanguage :: Language BInstruction
bLanguage = Language BParser.bParseString bInterpreter BCompiler.compileProgram

bInterpreter :: [BInstruction] -> String -> Maybe String
bInterpreter = undefined

-- bInterpreter instrs inputs =
--   case outputOf (BStepper.runProgram instrs) inputs of
--     Left err -> Nothing
--     Right s -> Just s

-- class Language a where
--   parse :: String -> Maybe [a]
--   interpret :: [a] -> String -> Maybe String
--   compile :: [a] -> [AInstruction]

-- instance Language WCommand where
--   parse :: String -> Maybe [WCommand]
--   parse = WParser.wParseString

--   interpret :: [WCommand] -> String -> Maybe String
--   interpret commands inputs = do
--     instrs <- mkProgram commands
--     case outputOf (runProgram instrs) inputs of
--       Left err -> Nothing
--       Right s -> Just s

--   compile :: [WCommand] -> [AInstruction]
--   compile = WCompiler.compileProgram

createInputFile :: FilePath -> String -> IO ()
createInputFile dir = writeFile (dir ++ "/in.txt")

readOutputFile :: FilePath -> IO String
readOutputFile dir = readFile (dir ++ "/out.txt")

-- createInputFile :: String ->