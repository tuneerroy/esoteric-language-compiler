module Arm64ToStringTest where

import ASyntax (AInstruction (..), BranchCond (..), Reg32 (..), Reg64 (..), toArm64String)
import Test.HUnit (Test (TestList), (~:), (~?=))

tArm64ToString :: Test
tArm64ToString =
  "arm64ToString"
    ~: TestList
      [ toArm64String
          (Mov (Reg 0) (Reg 1))
          ~?= "mov x0, x1",
        toArm64String
          (MovI (Reg 2) 5)
          ~?= "mov x2, #5",
        toArm64String
          (AAdd (Reg 0) (Reg 22) (Reg 2))
          ~?= "add x0, x22, x2",
        toArm64String
          (AAddI (Reg 0) (Reg 22) 3)
          ~?= "add x0, x22, #3",
        toArm64String
          (ASub (Reg 5) (Reg 11) (Reg 2))
          ~?= "sub x5, x11, x2",
        toArm64String
          (ASubI (Reg 0) (Reg 0) 16)
          ~?= "sub x0, x0, #16",
        toArm64String
          (AMul (Reg 0) (Reg 0) (Reg 2))
          ~?= "mul x0, x0, x2",
        toArm64String
          (Udiv (Reg 5) (Reg 11) (Reg 18))
          ~?= "sdiv x5, x11, x18",
        toArm64String
          (Madd (Reg 5) (Reg 11) (Reg 18) (Reg 12))
          ~?= "madd x5, x11, x18, x12",
        toArm64String
          (Msub (Reg 5) (Reg 11) (Reg 18) (Reg 12))
          ~?= "msub x5, x11, x18, x12",
        toArm64String
          (Negs (Reg 0) (Reg 1))
          ~?= "negs x0, x1",
        toArm64String
          (Psh (Reg 0))
          ~?= "str x0, [sp, #-16]!",
        toArm64String
          (GetAddress (Reg 0) "buf")
          ~?= "adrp x0, buf@page\nadd x0, x0, buf@pageoff",
        toArm64String
          (Ldr (Reg 0) (Reg 1) (-4))
          ~?= "ldr x0, [x1], #-4",
        toArm64String
          (LdrPreOff (Reg 0) (Reg 1) (-4))
          ~?= "ldr x0, [x1, #-4]",
        toArm64String
          (Ldrb (Reg32 0) (Reg 1) (Reg 2))
          ~?= "ldrb w0, [x1, x2]",
        toArm64String
          (LdrO (Reg 0) (Reg 1) (Reg 2))
          ~?= "ldr x0, [x1, x2]",
        toArm64String
          (StrO (Reg 0) (Reg 1) (Reg 2))
          ~?= "str x0, [x1, x2]",
        toArm64String
          (Str (Reg 0) (Reg 1))
          ~?= "str x0, [x1]",
        toArm64String
          (ALabel "test_label")
          ~?= "test_label:",
        toArm64String
          (Bl "test_label")
          ~?= "bl test_label",
        toArm64String
          (B ASyntax.None "test_label")
          ~?= "b test_label",
        toArm64String
          (B ASyntax.NE "test_label")
          ~?= "bne test_label",
        toArm64String
          (B ASyntax.EQ "test_label")
          ~?= "beq test_label",
        toArm64String
          (B ASyntax.LT "test_label")
          ~?= "blt test_label",
        toArm64String
          (B ASyntax.LE "test_label")
          ~?= "ble test_label",
        toArm64String
          (B ASyntax.GT "test_label")
          ~?= "bgt test_label",
        toArm64String
          (B ASyntax.GE "test_label")
          ~?= "bge test_label",
        toArm64String
          (Cmp (Reg 0) 4)
          ~?= "cmp x0, #4",
        toArm64String
          Ret
          ~?= "ret",
        toArm64String
          Svc
          ~?= "svc #0x80",
        toArm64String
          (Comment "comment here")
          ~?= "// comment here",
        toArm64String
          (Directive "data")
          ~?= ".data",
        toArm64String
          (Global "_start")
          ~?= ".global _start",
        toArm64String
          (Balign 16)
          ~?= ".balign 16",
        toArm64String
          (Allocate "label" 200)
          ~?= "label: .space 200, 0"
      ]