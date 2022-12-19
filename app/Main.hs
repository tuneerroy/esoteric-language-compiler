module Main where

import ASyntax (toArm64String)
-- import Compiler (compileProgram)
import BCompiler (compileProgram)
import BParser (parseString)
import Control.Monad (void, when)
import Data.List (intercalate)
import Data.Maybe (fromMaybe)
import Text.Read (readMaybe)
import WCompiler (compileProgram)
import WParser (parseString)

data Filetype = WS | BF

-- createUnitTest ::
--   String -> -- Directory Name, Ex: "examples"
--   String -> --Filename Extension, Ex: ".ws"
--   String -> -- Program Args, Ex: "12+23-*"
--   Language a ->
--   IO ()
-- createUnitTest dir ext args lang = do
--   putStrLn dir
--   let totalPath = dir ++ "/program"
--   program <- readFile (totalPath ++ "." ++ ext)
--   writeFile (dir ++ "/in.txt") args
--   putStrLn "here"
--   case parse lang program of
--     Nothing -> error "Unable to parse"
--     Just parsed -> do
--       -- avengers assemble
--       writeFile (totalPath ++ ".s") (instructionsToStrings $ compile lang parsed)
--       -- compile ARM assembly
--       putStrLn "here4"
--       runScript $ "as -o " ++ totalPath ++ ".o " ++ totalPath ++ ".s"
--       -- create executable
--       runScript $
--         "ld -macosx_version_min 11.0.0 -o "
--           ++ totalPath
--           ++ " "
--           ++ totalPath
--           ++ ".o -lSystem -syslibroot `xcrun -sdk macosx \
--              \ --show-sdk-path` -e _start -arch arm64"
--       -- run executable and output to "output.txt"
--       putStrLn "here5"
--       runScript $ "./" ++ totalPath ++ " > " ++ dir ++ "/out.txt"
--       actual <- readFile (dir ++ "/out.txt")
--       putStrLn "read file"
--       case interpret lang parsed args of
--         Nothing -> error "Unable to interpret"
--         Just expected -> do
--           putStr $ "Expected: <>" ++ expected ++ "<>\n"
--           putStr $ "Actual: <>" ++ actual ++ "<>\n"
--           assert $ expected == actual

main :: IO ()
main = do
  putStrLn "Input filename: "
  inputFilename <- getLine
  case tryOFileConversion inputFilename of
    Nothing -> do
      putStrLn "Invalid filename."
      return ()
    Just (outputFilename, filetype) -> do
      -- read program
      program <- readFile inputFilename
      case compile program filetype of
        Nothing -> putStrLn "Error parsing program."
        Just compiled ->
          writeFile outputFilename compiled
            >>= return (putStrLn "Compiled program")

tryOFileConversion :: String -> Maybe (String, Filetype)
tryOFileConversion (c : ".b") = Just (c : ".s", BF)
tryOFileConversion (c : ".ws") = Just (c : ".s", WS)
tryOFileConversion (c : cs) = tryOFileConversion cs >>= (\(cs', ft) -> return (c : cs', ft))
tryOFileConversion _ = Nothing

compile :: String -> Filetype -> Maybe String
compile s ft =
  case ft of
    WS -> do
      commands <- WParser.parseString s
      let assembly = WCompiler.compileProgram commands
      let assemblyStr = map toArm64String assembly
      return (intercalate "\n" assemblyStr)
    BF -> do
      commands <- BParser.parseString s
      let assembly = BCompiler.compileProgram commands
      let assemblyStr = map toArm64String assembly
      return (intercalate "\n" assemblyStr)

-- compile :: String -> Maybe String
-- compile s = do
--   commands <- parseString s
--   return (intercalate "\n" $ compileProgram commands)

-- compile :: String -> Maybe String
-- compile s = do
--   commands <- WParser.parseString s
--   let assembly = WCompiler.compileProgram commands
--   -- commands <- BParser.parseString s
--   -- let assembly = BCompiler.compileProgram commands
--   let assemblyStr = map toArm64String assembly
--   return (intercalate "\n" assemblyStr)
