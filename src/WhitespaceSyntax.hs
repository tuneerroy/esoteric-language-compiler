module WhitespaceSyntax where

data Token = Space | Tab | LF deriving (Eq, Show)
type Val = Int
type Label = [Token]

data Command
  = InputChar | InputNum | OutputChar | OutputNum
  | Push Val | Dup | Swap | Discard
  | Add | Sub | Mul | Div | Mod
  | Mark Label | CallSub Label | Jump Label | BranchZ Label | BranchN Label | EndSub | EndProg
  | Store | Retrieve

type Block = [Command]
