module Compiler where

import ASyntax (AInstruction)
import WSyntax (WInstruction)

compileCommand :: Eq a => WInstruction a -> [AInstruction String]
compileCommand = undefined

compileProgram :: Eq a => [WInstruction a] -> [AInstruction String]
compileProgram = concatMap compileCommand