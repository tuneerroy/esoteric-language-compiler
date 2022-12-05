module Compiler where
import WSyntax (WInstruction)
import ASyntax (AInstruction)

compileCommand :: Eq a => WInstruction a -> [AInstruction String]
compileCommand = undefined

compileProgram :: Eq a => [WInstruction a] -> [AInstruction String]
compileProgram = concatMap compileCommand