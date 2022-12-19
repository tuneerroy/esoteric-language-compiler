module BCompiler where

import ASyntax (AInstruction (..), BranchCond (..), Reg32 (..), Reg64 (..))
import BSyntax (BInstruction (..))

compileCommand :: (BInstruction, String) -> [AInstruction]
compileCommand (ins, idx) = case ins of
  IncrPtr ->
    [ AAddI (Reg 29) (Reg 29) 1
    ]
  DecrPtr ->
    [ ASubI (Reg 29) (Reg 29) 1
    ]
  IncrByte ->
    [ Ldrb (Reg32 0) (Reg 29) (Reg 28),
      AAddI (Reg 0) (Reg 0) 1,
      Strb (Reg32 0) (Reg 29)
    ]
  DecrByte ->
    [ Ldrb (Reg32 0) (Reg 29) (Reg 28),
      ASubI (Reg 0) (Reg 0) 1,
      Strb (Reg32 0) (Reg 29)
    ]
  Output ->
    [ Ldrb (Reg32 0) (Reg 29) (Reg 28),
      Psh (Reg 0),
      Bl "_output_char",
      Ldr (Reg 1) SP 16
    ]
  Input ->
    [ Ldrb (Reg32 0) (Reg 29) (Reg 30),
      Psh (Reg 0),
      Bl "_input_char",
      Ldr (Reg 1) SP 16
    ]
  While b ->
    [ ALabel ("while" ++ idx),
      Ldrb (Reg32 0) (Reg 29) (Reg 30),
      Cmp (Reg 0) 0,
      B NE ("whileend" ++ idx)
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
    MovI (Reg 2) 2,
    MovI (Reg 16) 3,
    Svc,
    Ldr (Reg 0) (Reg 26) 0,
    Ldr (Reg 1) SP 16,
    GetAddress (Reg 2) "buf",
    StrO (Reg 0) (Reg 2) (Reg 1),
    Ret,
    ALabel "_start",
    MovI (Reg 28) 0,
    GetAddress (Reg 29) "array"
  ]

footer :: [AInstruction]
footer =
  [ MovI (Reg 0) 0,
    MovI (Reg 16) 1,
    Svc
  ]

compileProgram :: [BInstruction] -> [AInstruction]
compileProgram a = header ++ concatMap compileCommand (zip a (map show [0 ..])) ++ footer