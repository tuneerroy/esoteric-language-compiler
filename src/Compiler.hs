module Compiler where

import ASyntax ()
import Data.List (intercalate)
import Data.List.NonEmpty (NonEmpty ((:|)))
import Test.HUnit (Test (..), (~:), (~?=))
import WParser (Token (..), WLabel (..))
import WSyntax
  ( WBop (..),
    WCond (Any, Neg, Zero),
    WInstruction (..),
  )

compileCommand :: (Eq a, NonWhitespaceShow a) => WInstruction a -> [String]
compileCommand i = case i of
  InputChar ->
    [ "; input character",
      "LDR r0, =charFormat",
      "LDR r1, =inputRead",
      "BL scanf",
      "PUSH {r1}"
    ]
  InputNum ->
    [ "; input number",
      "LDR r0, =numFormat",
      "LDR r1, =inputRead",
      "BL scanf",
      "PUSH {r1}"
    ]
  OutputChar ->
    [ "; output character",
      "MOV r0, sp",
      "BL printf"
    ]
  OutputNum ->
    [ "; output number",
      "MOV r0, sp",
      "BL printf"
    ]
  Push n ->
    [ "; push",
      "PUSH {" ++ show n ++ "}"
    ]
  Dup ->
    [ "; dup",
      "LDR r1, [sp]",
      "SUB sp, sp, #4",
      "STR r1, [sp]"
    ]
  Swap ->
    [ "; swap",
      "POP {r1, r2}",
      "PUSH {r2, r1}"
    ]
  Discard ->
    [ "; discard",
      "ADD sp, sp, #4"
    ]
  Copy n ->
    -- assumed top down nth
    [ "; copy",
      "MOVE r1 #" ++ show (n * 4), -- offset
      "LDR r2, [sp, r1]",
      "PUSH {r2}"
    ]
  Slide n ->
    [ "; slide",
      "ADD sp, sp, #" ++ show (n * 4)
    ]
  Arith Add ->
    [ "; add arith",
      "POP {r1, r2}",
      "ADD r3, r1, r2",
      -- not sure if these are for signed/unsigned operationz
      "PUSH {r3}"
    ]
  Arith Sub ->
    [ "; sub arith",
      "POP {r1, r2}",
      "SUB r3, r1, r2",
      -- not sure if these are for signed/unsigned operationz
      "PUSH {r3}"
    ]
  Arith wb ->
    -- IDK HOW TO DO THIS, HALPPPPPPPPPPPPPP x10 S.O.S.
    [ -- TODO: handle division & mod, -- ask Joe
      "; arith",
      "PUSH {r3}"
    ]
  Label a ->
    [ "; label",
      toString a
    ]
  Call a ->
    [ "; call",
      "BL " ++ toString a
    ]
  Branch Any a ->
    [ "; branch",
      "B " ++ toString a
    ]
  Branch Zero a ->
    [ "; branch",
      "CMP r1, #0",
      "BEQ " ++ toString a
    ]
  Branch Neg a ->
    [ "; branch",
      "CMP r1, #0",
      "BLT " ++ toString a
    ]
  Return ->
    [ "; return",
      "BX lr"
    ]
  End ->
    [ "; end",
      "B end"
    ]
  -- TODO: should this be replacing the memory values?
  Store ->
    [ "; store",
      "POP {r1, r2}", -- yes, I'm lazy
      "PUSH {r1, r2}",
      "STR r2, [r1]"
    ]
  Retrieve ->
    [ "; retrieve",
      "POP {r1, r2}", -- yes, I'm lazy
      "PUSH {r1, r2}",
      "LDR r2, [r1]"
    ]

ioPrep :: [String]
ioPrep =
  [ ".global printf",
    ".global scanf",
    ".data",
    ".balign 4",
    "inputRead: .word 0",
    ".balign 4",
    "numFormat: .asciz \"%d\"",
    ".balign 4",
    "charFormat: .asciz \"%c\""
  ]

compileProgram :: (Eq a, NonWhitespaceShow a) => [WInstruction a] -> [String]
compileProgram a = ioPrep ++ concatMap compileCommand a ++ ["end"]

class NonWhitespaceShow a where
  toString :: a -> String

instance NonWhitespaceShow WLabel where
  toString (WLabel t) = foldr (\x acc -> tokenToChar x : acc) "" t

tokenToChar :: Token -> Char
tokenToChar Space = 'S'
tokenToChar Tab = 'T'
tokenToChar LF = 'L'

-- TESTS
compileTest :: Test
compileTest =
  "compile tests"
    ~: TestList
      [ compileProgram ioProgram
          ~?= ["TODO: placeholder", "end"],
        compileProgram stackProgram
          ~?= [ "; push",
                "PUSH {5}",
                "; dup",
                "LDR r1, [sp]",
                "SUB sp, sp, #4",
                "STR r1, [sp]",
                "; swap",
                "POP {r1, r2}",
                "PUSH {r2, r1}",
                "; discard",
                "ADD sp, sp #4",
                "; copy",
                "MOVE r1 #8",
                "LDR r2, [sp, r1]",
                "PUSH {r2}",
                "; slide",
                "ADD sp, sp, #8",
                "end"
              ],
        compileProgram arithProgram
          ~?= [ "; add arith",
                "POP {r1, r2}",
                "ADD r3, r1, r2",
                "PUSH {r3}",
                "; sub arith",
                "POP {r1, r2}",
                "SUB r3, r1, r2",
                "PUSH {r3}",
                "TODO: undefined Mul", -- TODO
                "TODO: undefined Div", -- TODO
                "TODO: undefined Mod", -- TODO
                "end"
              ]
      ]

ioProgram :: [WInstruction WLabel]
ioProgram = undefined

stackProgram :: [WInstruction WLabel]
stackProgram = [Push 5, Dup, Swap, Discard, Copy 2, Slide 2]

arithProgram :: [WInstruction WLabel]
arithProgram = [Arith Add, Arith Sub, Arith Mul, Arith Div, Arith Mod]