module CompilerTest where

import System.Process (CreateProcess, createProcess, shell)
import Test.HUnit (Test, assert, runTestTT, (~:))

s :: CreateProcess
s = shell "bash script.sh"

-- >>> createProcess s

tHelloWorld :: Test
tHelloWorld =
  "hello world" ~: test2

test :: IO ()
test = do
  putStrLn "hello world"
  _ <- createProcess s
  program <- readFile "out.txt"
  putStrLn "aa"

test2 :: IO ()
test2 = do
  _ <- createProcess s
  str <- readFile "out.txt"
  assert (str == "Hola, mundo\n")

temp :: IO String
temp = readFile "out.txt"

-- >>> temp
-- "Hola, mundo\n"

-- >>> runTestTT tHelloWorld
-- Variable not in scope: runTestTT :: Test -> t
