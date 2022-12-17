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
    [ "// inputChar",
      "mov x0, #1              // fd = stdin",
      "adrp x1, buf@page",
      "add x1, x1, buf@pageoff // x1 is address of buf",
      "mov x26, x1",
      "mov x2, #2              // count = 2 (so it can read the \n)",
      "mov x16, #3             // unix read system call",
      "svc #0x80               // call kernel",
      "// now buf contains the value read in by the system call",
      "ldr x0, [x26]           // x0 = value read in",
      "ldr x1, [sp], #16       // push address from top of stack",
      "adrp x2, heap@page",
      "add x2, x2, heap@pageoff // x2 is address of heap",
      "str x0, [x2, x1]"
    ]
  InputNum ->
    [ "// inputNum",
      "bl _input_num"
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
    [ "// outputNum",
      "bl _output_num"
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
      "ldr x0, [sp, #" ++ show ((n - 1) * 16) ++ "]",
      "str x0, [sp, #-16]!"
    ]
  Slide n ->
    [ "// slide",
      "mov x0, #" ++ show n,
      "bl _slide"
    ]
  Arith Add ->
    [ "// add arith",
      "ldr x0, [sp], #16",
      "ldr x1, [sp], #16",
      "add x2, x0, x1",
      "str x2, [sp, #-16]!"
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
    [ "// branch",
      "b " ++ toString a
    ]
  Branch Zero a ->
    [ "// branch zero",
      "ldr x0, [sp]",
      "cmp x0, #0",
      "beq " ++ toString a
    ]
  Branch Neg a ->
    [ "// branch neg",
      "ldr x0, [sp]",
      "cmp x0, #0",
      "blt " ++ toString a
    ]
  Return ->
    [ "// return",
      "ret"
    ]
  End ->
    [ "// end",
      "b end"
    ]
  Store ->
    [ "// store",
      "ldr x0, [sp], #16 // value",
      "ldr x1, [sp], #16 // address",
      "adrp x2, heap@page",
      "add x2, x2, heap@pageoff",
      "str x0, [x2, x1]"
    ]
  Retrieve ->
    [ "// retrieve",
      "ldr x1, [sp], #16 // address",
      "adrp x2, heap@page",
      "add x2, x2, heap@pageoff",
      "ldr x0, [x2, x1]",
      "str x0, [sp, #-16]!"
    ]

ioPrep :: [String]
ioPrep =
  [ ".data",
    ".balign 4",
    "buf: .ds 4 // memory buffer for IO",
    ".balign 4",
    "heap: .space 1000000, 0",
    ".text",
    ".global _start",
    ".align 4",
    "_divide:",
    "udiv x2, x0, x1",
    "msub x3, x2, x1, x0",
    "ret",
    "_output_char:",
    "mov x0, #1",
    "adrp x1, buf@page",
    "add x1, x1, buf@pageoff",
    "ldr x8, [sp]",
    "str x8, [x1]",
    "mov x2, #1",
    "mov x16, #4",
    "svc #0x80",
    "ret",
    "_output_num:",
    "ldr x0, [sp]",
    "str x30, [sp, #-16]! // push x30 onto stack",
    "_int_to_ascii:",
    "cmp x0, #0",
    "b.ge _int_to_ascii_pos",
    "mov x1, #45 // ascii for '-'",
    "str x1, [sp, #-16]!",
    "mov x27, x0",
    "bl _output_char",
    "ldr x28, [sp], #16",
    "mov x0, x27",
    "mov x27, #0",
    "negs x0, x0",
    "_int_to_ascii_pos:",
    "// expects x0 = int",
    "cmp x0, #10",
    "b.lt _int_to_ascii_br",
    "mov x1, #10             // if x0 >= 10",
    "bl _divide",
    "mov x0, x2",
    "add x3, x3, #48",
    "str x3, [sp, #-16]!",
    "add x27, x27, #1",
    "bl _int_to_ascii_pos",
    "_int_to_ascii_br:      // if x0 < 10",
    "add x8, x0, #48         // add ascii representation of x0 to stack",
    "str x8, [sp, #-16]!",
    "add x27, x27, #1",
    "// print the stack x27 times",
    "_print_stack:",
    "cmp x27, #0             // if x27 <= 0",
    "b.le _done_ascii",
    "bl _output_char          // print top of stack",
    "ldr x28, [sp], #16",
    "sub x27, x27, #1",
    "bl _print_stack",
    "_done_ascii:",
    "ldr x30, [sp], #16 // load linked address back to x30",
    "ret",
    "_slide:",
    "// expects x0 = number of things to slide off",
    "ldr x1, [sp], #16",
    "mov x29, x30",
    "slide_loop:",
    "cmp x0, #0",
    "b.le slide",
    "ldr x28, [sp], #16",
    "sub x0, x0, #1",
    "bl slide_loop",
    "slide:",
    "str x1, [sp, #-16]!",
    "mov x30, x29",
    "ret",
    "_input_num:",
    "mov x22, x30            // store address",
    "mov x0, #0              // fd = stdin",
    "adrp x26, buf@page",
    "add x26, x26, buf@pageoff // x26 is address of buf",
    "mov x1, x26",
    "mov x2, #11             // count = 11 (since max int is 2147483647)",
    "mov x16, #3             // unix read system call",
    "svc #0x80               // call kernel",
    "// now buf contains the value read in by the system call",
    "// now x0 contains how many bytes were read (including the \\n)",
    "sub x0, x0, #1",
    "mov x25, x0             // x25 = num bytes read",
    "mov x24, x0             // x24 = num bytes read",
    "mov x23, #0             // x23 = where the number will be stored",
    "ldrb w1, [x26, x25]     // w1 = last character read",
    "cmp x1, #10             // check if x1 = '\\n'",
    "bne _no_nl",
    "sub x25, x25, #1        // ignore the '\\n' if it appears",
    "_no_nl:",
    "cmp x25, #0",
    "blt _calculate_number   // number is on stack, we now need to calculate it",
    "ldrb w0, [x26, x25]",
    "sub x0, x0, #48        // convert ascii",
    "str x0, [sp, #-16]!",
    "sub x25, x25, #1 ",
    "bl _no_nl",
    "_calculate_number:",
    "cmp x24, #0",
    "beq _done_calculation",
    "ldr x0, [sp], #16       // push top of stack to x0",
    "mov x1, #10",
    "madd x23, x23, x1, x0",
    "sub x24, x24, #1",
    "bl _calculate_number",
    "_done_calculation:",
    "ldr x1, [sp], #16       // push address from top of stack",
    "adrp x2, heap@page",
    "add x2, x2, heap@pageoff",
    "str x23, [x2, x1]       // store number in location given by top of the stack",
    "mov x30, x22",
    "ret",
    "_start:"
  ]

endPrep :: [String]
endPrep =
  [ "end:",
    "mov x0, #0",
    "mov x16, #1",
    "svc #0x80"
  ]

compileProgram :: (Eq a, NonWhitespaceShow a) => [WInstruction a] -> [String]
compileProgram a = ioPrep ++ concatMap compileCommand a ++ endPrep

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
