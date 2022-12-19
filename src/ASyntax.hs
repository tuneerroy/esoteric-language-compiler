module ASyntax where

import Data.Function ((&))
import Data.List (intercalate)
import Test.HUnit (Test (TestList), runTestTT, (~:), (~?=))

data Reg64 = Reg Int | SP -- between 0 and 31 or SP

newtype Reg32 = Reg32 Int

type Imm = Int

-- None means branch always
data BranchCond = None | NE | EQ | LT | LE | GT | GE

data AInstruction
  = -- Mov
    Mov Reg64 Reg64 -- mov Rd, Rt
  | MovI Reg64 Imm -- mov Rd, #imm
  -- Arith
  | AAdd Reg64 Reg64 Reg64 -- add Rd, Rt, Rs
  | AAddI Reg64 Reg64 Imm -- add Rd, Rt, #imm
  | ASub Reg64 Reg64 Reg64 -- sub Rd, Rt, Rs
  | ASubI Reg64 Reg64 Imm -- sub Rd, Rt, #imm
  | AMul Reg64 Reg64 Reg64 -- mul Rd, Rt, Rs
  | Udiv Reg64 Reg64 Reg64 -- udiv Rd, Rt, Rs
  | Madd Reg64 Reg64 Reg64 Reg64 -- madd Rd, Rt, Rs, Rr
  | Msub Reg64 Reg64 Reg64 Reg64 -- msub Rd, Rt, Rs, Rr
  | Negs Reg64 Reg64 -- negs Rd, Rt
  -- Stack
  | Psh Reg64 -- str Rt, [sp, #-16]!  (pushes the value in Rt to the stack)
  -- Addressing
  | GetAddress Reg64 String -- adrp Rd, label@page AND add Rd, Rd, label@pageoff
  | Ldr Reg64 Reg64 Imm -- ldr Rd, [Rt], #imm
  | LdrPreOff Reg64 Reg64 Imm -- Ldr Rd, [Rt, #imm]
  | Ldrb Reg32 Reg64 Reg64 -- ldrb wd, [Rt, Rs]    (note the destination is a 32 bit register)
  | LdrO Reg64 Reg64 Reg64 -- ldr Rd, [Rt, Rs]     (Rt is address, Rs holds offset)
  | StrO Reg64 Reg64 Reg64 -- str Rd, [Rt, Rs]     (Rt is address, Rs holds offset)
  | Str Reg64 Reg64 -- str Rd, [Rt]
  | Strb Reg32 Reg64 -- strb wd, [Rt]
  -- Flow control
  | ALabel String -- label:
  | Bl String -- bl label
  | B BranchCond String -- b(cond) label
  | Cmp Reg64 Imm -- cmp Rt, #imm
  | Ret -- ret
  | Svc -- svc #0x80
  -- Comment
  | Comment String -- // comment
  -- Misc directives
  | Directive String -- .l (ex: .data or .text)
  | Global String -- .global _start
  | Balign Int -- .balign i
  | Allocate String Int -- label: .space i, 0 (i is number of bytes we want allocated)

-- Function to convert our assembly syntax to a string containing the assembly command
toArm64String :: AInstruction -> String
toArm64String s = case s of
  Mov rd rt ->
    "mov "
      ++ reg rd
      ++ ", "
      ++ reg rt
  MovI rd imm ->
    "mov "
      ++ reg rd
      ++ ", #"
      ++ show imm
  AAdd rd rt rs ->
    "add "
      ++ reg rd
      ++ ", "
      ++ reg rt
      ++ ", "
      ++ reg rs
  AAddI rd rt imm ->
    "add "
      ++ reg rd
      ++ ", "
      ++ reg rt
      ++ ", #"
      ++ show imm
  ASub rd rt rs ->
    "sub "
      ++ reg rd
      ++ ", "
      ++ reg rt
      ++ ", "
      ++ reg rs
  ASubI rd rt imm ->
    "sub "
      ++ reg rd
      ++ ", "
      ++ reg rt
      ++ ", #"
      ++ show imm
  AMul rd rt rs ->
    "mul "
      ++ reg rd
      ++ ", "
      ++ reg rt
      ++ ", "
      ++ reg rs
  Udiv rd rt rs ->
    "sdiv "
      ++ reg rd
      ++ ", "
      ++ reg rt
      ++ ", "
      ++ reg rs
  Madd rd rt rs rr ->
    "madd "
      ++ reg rd
      ++ ", "
      ++ reg rt
      ++ ", "
      ++ reg rs
      ++ ", "
      ++ reg rr
  Msub rd rt rs rr ->
    "msub "
      ++ reg rd
      ++ ", "
      ++ reg rt
      ++ ", "
      ++ reg rs
      ++ ", "
      ++ reg rr
  Negs rd rt ->
    "negs "
      ++ reg rd
      ++ ", "
      ++ reg rt
  Psh rt ->
    "str "
      ++ reg rt
      ++ ", [sp, #-16]!"
  GetAddress rd label ->
    "adrp "
      ++ reg rd
      ++ ", "
      ++ label
      ++ "@page\nadd "
      ++ reg rd
      ++ ", "
      ++ reg rd
      ++ ", "
      ++ label
      ++ "@pageoff"
  Ldr rd rt imm ->
    "ldr "
      ++ reg rd
      ++ ", ["
      ++ reg rt
      ++ "], #"
      ++ show imm
  LdrPreOff rd rt imm ->
    "ldr "
      ++ reg rd
      ++ ", ["
      ++ reg rt
      ++ ", #"
      ++ show imm
      ++ "]"
  Ldrb (Reg32 i) rt rs ->
    "ldrb w"
      ++ show i
      ++ ", ["
      ++ reg rt
      ++ ", "
      ++ reg rs
      ++ "]"
  LdrO rd rt rs ->
    "ldr "
      ++ reg rd
      ++ ", ["
      ++ reg rt
      ++ ", "
      ++ reg rs
      ++ "]"
  StrO rd rt rs ->
    "str "
      ++ reg rd
      ++ ", ["
      ++ reg rt
      ++ ", "
      ++ reg rs
      ++ "]"
  Str rd rt ->
    "str "
      ++ reg rd
      ++ ", ["
      ++ reg rt
      ++ "]"
  Strb (Reg32 i) rt ->
    "strb w"
      ++ show i
      ++ ", ["
      ++ reg rt
      ++ "]"
  ALabel label ->
    label
      ++ ":"
  Bl label ->
    "bl "
      ++ label
  B cond label ->
    "b"
      ++ condToStr cond
      ++ " "
      ++ label
  Cmp rt imm ->
    "cmp "
      ++ reg rt
      ++ ", #"
      ++ show imm
  Ret -> "ret"
  Svc -> "svc #0x80"
  Comment com ->
    "// "
      ++ com
  Directive s ->
    "."
      ++ s
  Global s ->
    ".global "
      ++ s
  Balign i ->
    ".balign "
      ++ show i
  Allocate label i ->
    label
      ++ ": .space "
      ++ show i
      ++ ", 0"
  where
    reg :: Reg64 -> String
    reg (Reg i) = "x" ++ show i
    reg SP = "sp"
    condToStr :: BranchCond -> String
    condToStr ASyntax.None = ""
    condToStr ASyntax.NE = "ne"
    condToStr ASyntax.EQ = "eq"
    condToStr ASyntax.LT = "lt"
    condToStr ASyntax.LE = "le"
    condToStr ASyntax.GT = "gt"
    condToStr ASyntax.GE = "ge"

-- TESTING
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
          ~?= "udiv x5, x11, x18",
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

instructionsToStrings :: [AInstruction] -> String
instructionsToStrings as = map toArm64String as & intercalate "\n"

-- >>> runTestTT tArm64ToString
-- Counts {cases = 36, tried = 36, errors = 0, failures = 1}
