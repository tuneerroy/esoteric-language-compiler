module WCompiler (compileProgram) where

import ASyntax (AInstruction (..), BranchCond (..), Reg32 (..), Reg64 (..))
import WParser (Token (..), WLabel (..))
import WSyntax (WBop (..), WCond (Neg, Zero), WInstruction (..))

-- Compiles a single whitespace command to a list of assembly instructions
compileCommand :: (Eq a, NonWhitespaceShow a) => WInstruction a -> [AInstruction]
compileCommand i = case i of
  InputChar ->
    [ Comment "inputChar",
      MovI (Reg 0) 1,
      GetAddress (Reg 1) "buf",
      Mov (Reg 26) (Reg 1),
      MovI (Reg 2) 1,
      MovI (Reg 16) 3,
      Svc,
      Ldr (Reg 0) (Reg 26) 0,
      Ldr (Reg 1) SP 16,
      GetAddress (Reg 2) "heap",
      MovI (Reg 3) 8,
      AMul (Reg 1) (Reg 1) (Reg 3),
      StrO (Reg 0) (Reg 2) (Reg 1)
    ]
  InputNum ->
    [ Comment "inputNum",
      Bl "_input_num"
    ]
  OutputChar ->
    [ Comment "outputChar",
      Bl "_output_char"
    ]
  OutputNum ->
    [ Comment "outputNum",
      Bl "_output_num"
    ]
  Push n ->
    [ Comment "push",
      MovI (Reg 0) n,
      Psh (Reg 0)
    ]
  Dup ->
    [ Comment "dup",
      Ldr (Reg 0) SP 16,
      Psh (Reg 0),
      Psh (Reg 0)
    ]
  Swap ->
    [ Comment "swap",
      Ldr (Reg 0) SP 16,
      Ldr (Reg 1) SP 16,
      Psh (Reg 0),
      Psh (Reg 1)
    ]
  Discard ->
    [ Comment "discard",
      Ldr (Reg 0) SP 16
    ]
  Copy n ->
    [ Comment "copy",
      LdrPreOff (Reg 0) SP (fromEnum n * 16),
      Psh (Reg 0)
    ]
  Slide n ->
    [ Comment "slide",
      MovI (Reg 0) (fromEnum n),
      Bl "_slide"
    ]
  Arith Add ->
    [ Comment "add",
      Ldr (Reg 0) SP 16,
      Ldr (Reg 1) SP 16,
      AAdd (Reg 2) (Reg 1) (Reg 0),
      Psh (Reg 2)
    ]
  Arith Sub ->
    [ Comment "sub",
      Ldr (Reg 0) SP 16,
      Ldr (Reg 1) SP 16,
      ASub (Reg 2) (Reg 1) (Reg 0),
      Psh (Reg 2)
    ]
  Arith Mul ->
    [ Comment "mul",
      Ldr (Reg 0) SP 16,
      Ldr (Reg 1) SP 16,
      AMul (Reg 2) (Reg 1) (Reg 0),
      Psh (Reg 2)
    ]
  Arith Div ->
    [ Comment "div",
      Ldr (Reg 1) SP 16,
      Ldr (Reg 0) SP 16,
      Bl "_divide",
      Psh (Reg 2)
    ]
  Arith Mod ->
    [ Comment "div",
      Ldr (Reg 1) SP 16,
      Ldr (Reg 0) SP 16,
      Bl "_divide",
      Psh (Reg 3)
    ]
  Label a ->
    [ Comment "label",
      ALabel (toString a)
    ]
  Call a ->
    [ Comment "call",
      Adr (Reg 0),
      AAddI (Reg 20) (Reg 0) 12,
      Bl (toString a),
      MovI (Reg 20) 0
    ]
  Jump a ->
    [ Comment "branch",
      B ASyntax.None (toString a)
    ]
  Branch Zero a ->
    [ Comment "branch zero",
      Ldr (Reg 0) SP 16,
      Cmp (Reg 0) 0,
      B ASyntax.EQ (toString a)
    ]
  Branch Neg a ->
    [ Comment "branch neg",
      Ldr (Reg 0) SP 16,
      Cmp (Reg 0) 0,
      B ASyntax.LT (toString a)
    ]
  Return ->
    [ Comment "return",
      Mov (Reg 30) (Reg 20),
      Ret
    ]
  End ->
    [ Comment "end",
      B None "end"
    ]
  Store ->
    [ Comment "store",
      Ldr (Reg 0) SP 16,
      Ldr (Reg 1) SP 16,
      MovI (Reg 3) 8,
      GetAddress (Reg 2) "heap",
      AMul (Reg 1) (Reg 1) (Reg 3),
      StrO (Reg 0) (Reg 2) (Reg 1)
    ]
  Retrieve ->
    [ Comment "retrieve",
      Ldr (Reg 1) SP 16,
      MovI (Reg 3) 8,
      AMul (Reg 1) (Reg 1) (Reg 3),
      GetAddress (Reg 2) "heap",
      LdrO (Reg 0) (Reg 2) (Reg 1),
      Psh (Reg 0)
    ]

-- Data directives and subroutines we may use in every program
header :: [AInstruction]
header =
  [ Directive "data",
    Balign 4,
    Allocate "buf" 20,
    Balign 4,
    Allocate "heap" 10000000,
    Directive "text",
    Global "_start",
    Balign 16,
    ALabel "_divide",
    Udiv (Reg 2) (Reg 0) (Reg 1),
    Msub (Reg 3) (Reg 2) (Reg 1) (Reg 0),
    Ret,
    ALabel "_output_char",
    MovI (Reg 0) 1,
    GetAddress (Reg 1) "buf",
    Ldr (Reg 8) SP 16,
    Str (Reg 8) (Reg 1),
    MovI (Reg 2) 1,
    MovI (Reg 16) 4,
    Svc,
    Ret,
    ALabel "_output_num",
    Ldr (Reg 0) SP 16,
    Psh (Reg 30),
    ALabel "_int_to_ascii",
    Cmp (Reg 0) 0,
    B GE "_int_to_ascii_pos",
    MovI (Reg 1) 45,
    Psh (Reg 1),
    Mov (Reg 27) (Reg 0),
    Bl "_output_char",
    Mov (Reg 0) (Reg 27),
    MovI (Reg 27) 0,
    Negs (Reg 0) (Reg 0),
    ALabel "_int_to_ascii_pos",
    Cmp (Reg 0) 10,
    B ASyntax.LT "_int_to_ascii_br",
    MovI (Reg 1) 10,
    Bl "_divide",
    Mov (Reg 0) (Reg 2),
    AAddI (Reg 3) (Reg 3) 48,
    Psh (Reg 3),
    AAddI (Reg 27) (Reg 27) 1,
    Bl "_int_to_ascii_pos",
    ALabel "_int_to_ascii_br",
    AAddI (Reg 8) (Reg 0) 48,
    Psh (Reg 8),
    AAddI (Reg 27) (Reg 27) 1,
    ALabel "_print_stack",
    Cmp (Reg 27) 0,
    B LE "_done_ascii",
    Bl "_output_char",
    ASubI (Reg 27) (Reg 27) 1,
    Bl "_print_stack",
    ALabel "_done_ascii",
    Ldr (Reg 30) SP 16,
    Ret,
    ALabel "_slide",
    Ldr (Reg 1) SP 16,
    Mov (Reg 29) (Reg 30),
    ALabel "slide_loop",
    Cmp (Reg 0) 0,
    B LE "slide",
    Ldr (Reg 28) SP 16,
    ASubI (Reg 0) (Reg 0) 1,
    Bl "slide_loop",
    ALabel "slide",
    Psh (Reg 1),
    Mov (Reg 30) (Reg 29),
    Ret,
    ALabel "_input_num",
    MovI (Reg 21) 1,
    Mov (Reg 22) (Reg 30),
    MovI (Reg 0) 0,
    GetAddress (Reg 26) "buf",
    Mov (Reg 1) (Reg 26),
    MovI (Reg 2) 11,
    MovI (Reg 16) 3,
    Svc,
    MovI (Reg 6) 0,
    Ldrb (Reg32 5) (Reg 26) (Reg 6),
    Cmp (Reg 5) 45,
    B NE "_nonneg",
    AAddI (Reg 26) (Reg 26) 1,
    ASubI (Reg 0) (Reg 0) 1,
    MovI (Reg 21) (-1),
    ALabel "_nonneg",
    ASubI (Reg 0) (Reg 0) 1,
    Mov (Reg 25) (Reg 0),
    Mov (Reg 24) (Reg 0),
    MovI (Reg 23) 0,
    Ldrb (Reg32 1) (Reg 26) (Reg 25),
    Cmp (Reg 1) 10,
    B NE "_no_nl",
    ASubI (Reg 25) (Reg 25) 1,
    ALabel "_no_nl",
    Cmp (Reg 25) 0,
    B ASyntax.LT "_calculate_number",
    Ldrb (Reg32 0) (Reg 26) (Reg 25),
    ASubI (Reg 0) (Reg 0) 48,
    Psh (Reg 0),
    ASubI (Reg 25) (Reg 25) 1,
    Bl "_no_nl",
    ALabel "_calculate_number",
    Cmp (Reg 24) 0,
    B ASyntax.EQ "_done_calculation",
    Ldr (Reg 0) SP 16,
    MovI (Reg 1) 10,
    Madd (Reg 23) (Reg 23) (Reg 1) (Reg 0),
    ASubI (Reg 24) (Reg 24) 1,
    Bl "_calculate_number",
    ALabel "_done_calculation",
    AMul (Reg 23) (Reg 23) (Reg 21),
    Ldr (Reg 1) SP 16,
    GetAddress (Reg 2) "heap",
    MovI (Reg 3) 8,
    AMul (Reg 1) (Reg 1) (Reg 3),
    StrO (Reg 23) (Reg 2) (Reg 1),
    Mov (Reg 30) (Reg 22),
    Ret,
    ALabel "_start"
  ]

footer :: [AInstruction]
footer =
  [ ALabel "end",
    MovI (Reg 0) 0,
    MovI (Reg 16) 1,
    Svc
  ]

compileProgram :: (Eq a, NonWhitespaceShow a) => [WInstruction a] -> [AInstruction]
compileProgram a = header ++ concatMap compileCommand a ++ footer

class NonWhitespaceShow a where
  toString :: a -> String

instance NonWhitespaceShow WLabel where
  toString (WLabel t) = foldr (\x acc -> tokenToChar x : acc) "" t

tokenToChar :: Token -> Char
tokenToChar Space = 'S'
tokenToChar Tab = 'T'
tokenToChar LF = 'L'
