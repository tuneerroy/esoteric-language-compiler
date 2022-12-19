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
--   commands <- parseString s
--   return (intercalate "\n" $ compileProgram commands)

compile :: String -> Maybe String
compile s = do
  commands <- WParser.parseString s
  let assembly = WCompiler.compileProgram commands
  -- commands <- BParser.parseString s
  -- let assembly = BCompiler.compileProgram commands
  let assemblyStr = map toArm64String assembly
  return (intercalate "\n" assemblyStr)
