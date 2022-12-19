module ASyntax (AInstruction (..), BranchCond (..), Reg32 (..), Reg64 (..), toArm64String, instructionsToStrings) where

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
  | Adr Reg64 -- Adr Rd, . (gets current address)
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
  Adr rd ->
    "adr "
      ++ reg rd
      ++ ", ."
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

instructionsToStrings :: [AInstruction] -> String
instructionsToStrings as = map toArm64String as & intercalate "\n"
