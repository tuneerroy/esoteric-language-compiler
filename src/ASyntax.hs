module ASyntax where

data Reg64 = Reg Int | SP -- between 0 and 31 or SP

newtype Reg32 = Reg32 Int

type Imm = Int

data Cond = None | NE | EQ | LT | LE | GT | GE

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
  | Ldrp Reg32 Reg64 Reg64 -- ldrp wd, [Rt, Rs]    (note the destination is a half register)
  | StrO Reg64 Reg64 Reg64 -- str Rd, [Rt, Rs]     (Rt is address, Rs holds offset)
  | Str Reg64 Reg64 -- str Rd, [Rt]
  -- Flow control
  | ALabel String -- label:
  | Bl String -- bl label
  | B Cond String -- b(cond) label
  | Cmp Reg64 Imm -- cmp Rd, #imm
  | Ret -- ret
  | Svc -- svc #0x80
  -- Comment
  | Comment String -- // comment
  -- preamble stuff
  | Directive String -- .l (ex: .data or .text)
  | Balign Int -- .balign i
  | Allocate String Int -- label: .space i, 0 (i is number of bytes we want allocated)