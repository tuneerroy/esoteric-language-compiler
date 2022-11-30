module Compiler where

import AssemblySyntax (ACommand)
import WhitespaceSyntax (WInstruction)

compileCommand :: Eq a => WInstruction a -> [ACommand String]
compileCommand = undefined

compileProgram :: Eq a => [WInstruction a] -> [ACommand String]
compileProgram = concatMap compileCommand