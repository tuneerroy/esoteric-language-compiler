module BCompiler where

import ASyntax (AInstruction (..), BranchCond (..), Reg32 (..), Reg64 (..), toArm64String)
import BSyntax (BInstruction (..))
import Data.List (intercalate)

compileCommand :: (BInstruction, String) -> [AInstruction]
compileCommand (ins, idx) = case ins of
  IncrPtr ->
    [ Comment "IncrPtr",
      Ldr (Reg 0) SP 16,
      AAddI (Reg 0) (Reg 0) 1,
      Psh (Reg 0)
    ]
  DecrPtr ->
    [ Comment "DecrPtr",
      Ldr (Reg 0) SP 16,
      ASubI (Reg 0) (Reg 0) 1,
      Psh (Reg 0)
    ]
  IncrByte ->
    [ Comment "IncrByte",
      Ldr (Reg 0) SP 0,
      Ldrb (Reg32 1) (Reg 29) (Reg 0),
      AAddI (Reg 1) (Reg 1) 1,
      AAdd (Reg 0) (Reg 0) (Reg 29),
      Strb (Reg32 1) (Reg 0)
    ]
  DecrByte ->
    [ Comment "DecrByte",
      Ldr (Reg 0) SP 0,
      Ldrb (Reg32 1) (Reg 29) (Reg 0),
      ASubI (Reg 1) (Reg 1) 1,
      AAdd (Reg 0) (Reg 0) (Reg 29),
      Strb (Reg32 1) (Reg 0)
    ]
  Output ->
    [ Comment "Output",
      Ldr (Reg 0) SP 0,
      Ldrb (Reg32 1) (Reg 29) (Reg 0),
      Psh (Reg 1),
      Bl "_output_char",
      Ldr (Reg 0) SP 16
    ]
  Input ->
    [ Comment "Input",
      Bl "_input_char"
    ]
  While b ->
    [ Comment "While",
      ALabel ("while" ++ idx),
      Ldr (Reg 0) SP 0,
      Ldrb (Reg32 1) (Reg 29) (Reg 0),
      Cmp (Reg 1) 0,
      B ASyntax.EQ ("whileend" ++ idx)
    ]
      ++ concatMap compileCommand (zip b (map f [0 ..]))
      ++ [ B None ("while" ++ idx),
           ALabel ("whileend" ++ idx)
         ]
    where
      f :: Int -> String
      f n = idx ++ "." ++ show n

header :: [AInstruction]
header =
  [ Directive "data",
    Balign 4,
    Allocate "buf" 20,
    Balign 4,
    Allocate "array" 30000,
    Directive "text",
    Global "_start",
    Balign 16,
    ALabel "_output_char",
    MovI (Reg 0) 1,
    GetAddress (Reg 1) "buf",
    Ldr (Reg 8) SP 0,
    Str (Reg 8) (Reg 1),
    MovI (Reg 2) 1,
    MovI (Reg 16) 4,
    Svc,
    Ret,
    ALabel "_input_char",
    MovI (Reg 0) 1,
    GetAddress (Reg 1) "buf",
    Mov (Reg 26) (Reg 1),
    MovI (Reg 2) 1,
    MovI (Reg 16) 3,
    Svc,
    Ldr (Reg 0) (Reg 26) 0,
    Ldr (Reg 1) SP 0,
    GetAddress (Reg 2) "array",
    StrO (Reg 0) (Reg 2) (Reg 1),
    Ret,
    ALabel "_start",
    GetAddress (Reg 29) "array",
    MovI (Reg 0) 0,
    Psh (Reg 0)
  ]

footer :: [AInstruction]
footer =
  [ MovI (Reg 0) 0,
    MovI (Reg 16) 1,
    Svc
  ]

compileProgram :: [BInstruction] -> [AInstruction]
compileProgram a = header ++ concatMap compileCommand (zip a (map show [0 ..])) ++ footer
