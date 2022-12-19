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
        Nothing -> putStrLn "Error parsing program"
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
      commands <- wParseString s
      let assembly = WCompiler.compileProgram commands
      let assemblyStr = map toArm64String assembly
      return (intercalate "\n" assemblyStr)
    BF -> do
      commands <- bParseString s
      let assembly = BCompiler.compileProgram commands
      let assemblyStr = map toArm64String assembly
      return (intercalate "\n" assemblyStr)

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
