module Main where

import Compiler (compileProgram)
import Control.Monad (void, when)
import Data.List (intercalate)
import Data.Maybe (fromMaybe)
import Text.Read (readMaybe)
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

compile :: String -> Maybe String
compile s = do
  commands <- wParseString s
  return (intercalate "\n" $ compileProgram commands)