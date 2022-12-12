module Main where

import Compiler (compileProgram)
import Data.List (intercalate)
import WParser (wParseString)

main :: IO ()
main = do
  putStrLn "Input filename: "
  inputFilename <- getLine
  putStrLn "Output filename: "
  outputFilename <- getLine

  program <- readFile inputFilename
  let res = compile program
  case res of
    Nothing -> putStrLn "Error parsing program"
    Just compiled ->
      writeFile outputFilename compiled
        >>= return (putStrLn "Compiled program")

compile :: String -> Maybe String
compile s = do
  commands <- wParseString s
  return (intercalate "\n" $ compileProgram commands)