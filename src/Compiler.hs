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
    [ "TODO"
    ]
  InputNum ->
    [ "TODO"
    ]
  OutputChar ->
    [ "// outputChar",
      "mov x0, #1",
      "adrp x1, buf@page",
      "add x1, x1, buf@pageoff",
      "ldr x8, [sp]",
      "str x8, [x1]",
      "mov x2, #1",
      "mov x16, #4",
      "svc #0x80"
    ]
  OutputNum ->
    -- TODO fix labels
    [ "// outputNum",
      "ldr x0, [sp]",
      "_int_to_ascii1:",
      "cmp x0, #0",
      "b.ge _int_to_ascii_pos1",
      "mov x1, #45 // ascii for '-'",
      "str x1, [sp, #-16]!",
      "mov x27, x0",
      "bl _ouput_char",
      "ldr x28, [sp], #16",
      "mov x0, x27",
      "mov x27, #0",
      "negs x0, x0",
      "_int_to_ascii_pos1:",
      "// expects x0 = int",
      "cmp x0, #10",
      "b.lt _int_to_ascii_br1",
      "mov x1, #10             // if x0 >= 10",
      "bl _divide",
      "mov x0, x2",
      "add x3, x3, #48",
      "str x3, [sp, #-16]!",
      "add x27, x27, #1",
      "bl _int_to_ascii_pos1",
      "_int_to_ascii_br1:      // if x0 < 10",
      "add x8, x0, #48         // add ascii representation of x0 to stack",
      "str x8, [sp, #-16]!",
      "add x27, x27, #1",
      "// print the stack x27 times",
      "_print_stack1:",
      "cmp x27, #0             // if x27 <= 0",
      "b.le _done_ascii1",
      "bl _ouput_char          // print top of stack",
      "ldr x28, [sp], #16",
      "sub x27, x27, #1",
      "bl _print_stack1",
      "_done_ascii1:"
    ]
  Push n ->
    [ "// push",
      "mov x0, #" ++ show n,
      "str x0, [sp, #-16]!"
    ]
  Dup ->
    [ "// dup",
      "ldr x0, [sp], #16",
      "str x0, [sp, #-16]!",
      "str x0, [sp, #-16]!"
    ]
  Swap ->
    [ "// swap",
      "ldr x0, [sp], #16",
      "ldr x1, [sp], #16",
      "str x0, [sp, #-16]!",
      "str x1, [sp, #-16]!"
    ]
  Discard ->
    [ "// discard",
      "ldr x0, [sp], #16"
    ]
  Copy n ->
    [ "// copy",
      "ldr x0, [sp, #" ++ show ((n - 1) * 16) ++ "]", -- offset
      "str x0, [sp, #-16]!"
    ]
  Slide n ->
    -- TODO fix labels
    [ "// slide",
      "mov x0, #" ++ show n,
      "ldr x1, [sp], #16",
      "slide_loop1:",
      "cmp x0, #0",
      "b.le slide1",
      "ldr x28, [sp], #16",
      "sub x0, x0, #1",
      "bl slide_loop1",
      "slide1:",
      "tr x1, [sp, #-16]!"
    ]
  Arith Add ->
    [ "// add arith",
      "ldr x0, [sp], #16",
      "ldr x1, [sp], #16",
      "add x2, x0, x1",
      "str x2, [sp, #-16]!"
      -- not sure if these are for signed/unsigned operationz
    ]
  Arith Sub ->
    [ "// sub arith",
      "ldr x0, [sp], #16",
      "ldr x1, [sp], #16",
      "sub x2, x0, x1",
      "str x2, [sp, #-16]!"
    ]
  Arith Mul ->
    [ "// mul arith",
      "ldr x0, [sp], #16",
      "ldr x1, [sp], #16",
      "mul x2, x0, x1",
      "str x2, [sp, #-16]!"
    ]
  Arith Div ->
    [ "// div arith",
      "ldr x0, [sp], #16",
      "ldr x1, [sp], #16",
      "bl _divide",
      "str x2, [sp, #-16]!"
    ]
  Arith Mod ->
    [ "// mod arith",
      "ldr x0, [sp], #16",
      "ldr x1, [sp], #16",
      "bl _divide",
      "str x3, [sp, #-16]!"
    ]
  Label a ->
    [ "// label",
      toString a ++ ":"
    ]
  Call a ->
    [ "// call",
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
  [ ".data",
    "buf: .ds 4 // memory buffer for IO",
    ".text",
    ".global _start",
    ".align 2",
    "_divide:",
    "udiv x2, x0, x1",
    "msub x3, x2, x1, x0",
    "ret",
    "_ouput_char:",
    "mov x0, #1",
    "adrp x1, buf@page",
    "add x1, x1, buf@pageoff",
    "ldr x8, [sp]",
    "str x8, [x1]",
    "mov x2, #1",
    "mov x16, #4",
    "svc #0x80",
    "ret",
    "_start:"
  ]

compileProgram :: (Eq a, NonWhitespaceShow a) => [WInstruction a] -> [String]
compileProgram a = ioPrep ++ concatMap compileCommand a ++ ["end:", "mov x0, #0", "mov x16, #1", "svc #0x80"]

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
