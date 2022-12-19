module Main where

import ASyntax (toArm64String)
-- import Compiler (compileProgram)

import BCompiler (compileProgram)
import BParser (bParseString)
import Control.Monad (void, when)
import Data.List (intercalate)
import Data.Maybe (fromMaybe)
import Text.Read (readMaybe)
import WCompiler (compileProgram)
import WParser (wParseString)

main :: IO ()
main = do
  putStrLn "Input filename: "
  inputFilename <- getLine
  case tryOFileConversion inputFilename of
    Nothing -> do
      putStrLn "Invalid filename."
      return ()
    Just outputFilename -> do
      -- read program
      program <- readFile inputFilename

      case compile program of
        Nothing -> putStrLn "Error parsing program"
        Just compiled ->
          writeFile outputFilename compiled
            >>= return (putStrLn "Compiled program")

tryOFileConversion :: String -> Maybe String
tryOFileConversion (c : ".ws") = Just $ c : ".s"
tryOFileConversion (c : cs) = tryOFileConversion cs >>= (\cs' -> return (c : cs'))
tryOFileConversion _ = Nothing

-- tryOFileConversion :: String -> Maybe String
-- tryOFileConversion (c : ".b") = Just $ c : ".s"
-- tryOFileConversion (c : cs) = tryOFileConversion cs >>= (\cs' -> return (c : cs'))
-- tryOFileConversion _ = Nothing

-- compile :: String -> Maybe String
-- compile s = do
--   commands <- wParseString s
--   return (intercalate "\n" $ compileProgram commands)

compile :: String -> Maybe String
compile s = do
  commands <- wParseString s
  let assembly = WCompiler.compileProgram commands
  -- commands <- bParseString s
  -- let assembly = BCompiler.compileProgram commands
  let assemblyStr = map toArm64String assembly
  return (intercalate "\n" assemblyStr)
