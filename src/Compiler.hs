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
      "ldr r0, =charFormat",
      "ldr r1, =inputRead",
      "bl scanf",
      "sub sp, sp, #4",
      "str r1, [sp]"
    ]
  InputNum ->
    [ "; input number",
      "ldr r0, =numFormat",
      "ldr r1, =inputRead",
      "bl scanf",
      "sub sp, sp, #4",
      "str r1, [sp]"
    ]
  OutputChar ->
    [ "; output character",
      "mov r0, sp",
      "bl printf"
    ]
  OutputNum ->
    [ "; output number",
      "mov r0, sp",
      "bl printf"
    ]
  Push n ->
    [ "; push",
      "mov r0, #" ++ show n,
      "sub sp, sp, #4",
      "str r0, [sp]"
    ]
  Dup ->
    [ "; dup",
      "ldr r1, [sp]",
      "sub sp, sp, #4",
      "str r1, [sp]"
    ]
  Swap ->
    [ "; swap",
      "ldr r0, [sp]",
      "add, sp, sp, #4",
      "ldr r1, [sp]",
      "str r0, [sp]",
      "sub sp, sp, #4",
      "ldr r0, [sp]"
    ]
  Discard ->
    [ "; discard",
      "add sp, sp, #4"
    ]
  Copy n ->
    -- assumed top down nth
    [ "; copy",
      "move r1 #" ++ show (n * 4), -- offset
      "ldr r2, [sp, r1]",
      "sub sp, sp, #4",
      "str r2, [sp]"
    ]
  Slide n ->
    [ "; slide",
      "add sp, sp, #" ++ show (n * 4)
    ]
  Arith Add ->
    [ "; add arith",
      "ldr r0, [sp]",
      "add, sp, sp, #4",
      "ldr r1, [sp]",
      "add r1, r1, r0",
      "str r1, [sp]"
      -- not sure if these are for signed/unsigned operationz
    ]
  Arith Sub ->
    [ "; sub arith",
      "ldr r0, [sp]",
      "add, sp, sp, #4",
      "ldr r1, [sp]",
      "sub r1, r1, r0",
      "str r1, [sp]"
      -- not sure if these are for signed/unsigned operationz
    ]
  Arith wb ->
    [ -- TODO: handle division & mod, -- ask Joe
      "; arith",
      "sub sp, sp, #4"
    ]
  Label a ->
    [ "; label",
      toString a ++ ":"
    ]
  Call a ->
    [ "; call",
      "bl " ++ toString a
    ]
  Branch Any a ->
    [ "; branch",
      "b " ++ toString a
    ]
  Branch Zero a ->
    [ "; branch",
      "cmp r1, #0",
      "beq " ++ toString a
    ]
  Branch Neg a ->
    [ "; branch",
      "cmp r1, #0",
      "blt " ++ toString a
    ]
  Return ->
    [ "; return",
      "bx lr"
    ]
  End ->
    [ "; end",
      "b end"
    ]
  -- TODO: should this be replacing the memory values?
  Store ->
    [ "; store",
      "mov r3, #0",
      "ldr r1, [sp, r3]",
      "mov r3, #4",
      "ldr r0, [sp, r3]",
      "str r1, [sp]"
    ]
  Retrieve ->
    [ "; retrieve",
      "ldr r0, [sp]",
      "ldr r0, [r0]",
      "sub sp, sp, #4",
      "str r0, [sp]"
    ]

ioPrep :: [String]
ioPrep =
  [ ".global printf",
    ".global scanf",
    ".data",
    ".align 2",
    "inputRead: .word 0",
    ".align 2",
    "numFormat: .asciz \"%d\"",
    ".align 2",
    "charFormat: .asciz \"%c\"",
    ".global _start",
    ".align 2",
    "_start:"
  ]

compileProgram :: (Eq a, NonWhitespaceShow a) => [WInstruction a] -> [String]
compileProgram a = ioPrep ++ concatMap compileCommand a ++ ["end:"]

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
                "push {5}",
                "; dup",
                "ldr r1, [sp]",
                "sub sp, sp, #4",
                "str r1, [sp]",
                "; swap",
                "pop {r1, r2}",
                "push {r2, r1}",
                "; discard",
                "add sp, sp #4",
                "; copy",
                "move r1 #8",
                "ldr r2, [sp, r1]",
                "push {r2}",
                "; slide",
                "add sp, sp, #8",
                "end"
              ],
        compileProgram arithProgram
          ~?= [ "; add arith",
                "pop {r1, r2}",
                "add r3, r1, r2",
                "push {r3}",
                "; sub arith",
                "pop {r1, r2}",
                "sub r3, r1, r2",
                "push {r3}",
                "TODO: undefined Mul", -- TODO
                "TODO: undefined Div", -- TODO
                "TODO: undefined Mod", -- TODO
                "end"
              ]
      ]

ioProgram :: [WInstruction WLabel]
ioProgram = [InputChar, InputNum, OutputChar, OutputNum]

stackProgram :: [WInstruction WLabel]
stackProgram = [Push 5, Dup, Swap, Discard, Copy 2, Slide 2]

arithProgram :: [WInstruction WLabel]
arithProgram = [Arith Add, Arith Sub, Arith Mul, Arith Div, Arith Mod]
