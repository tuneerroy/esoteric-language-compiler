module Compiler where
import WhitespaceSyntax (WInstruction)
import AssemblySyntax (ACommand)

compileCommand :: Eq a => WInstruction a -> [ACommand String]
compileCommand = undefined

compileProgram :: Eq a => [WInstruction a] -> [ACommand String]
compileProgram = concatMap compileCommand