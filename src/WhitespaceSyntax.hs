module WhitespaceSyntax (WBop(..), WCond(..), WVal, WInstruction(..)) where

data WBop = Add | Sub | Mul | Div | Mod deriving (Eq, Show)
data WCond = Any | Zero | Neg deriving (Eq, Show)
-- In the offical implementation, stack values are Integer
-- But that would be a pain to convert assembly to
type WVal = Int

data WInstruction l
-- IO
  = InputChar
  | InputNum
  | OutputChar
  | OutputNum
-- Stack
  | Push WVal
  | Dup
  | Swap
  | Discard
  | Copy WVal
  | Slide WVal
-- Arithmetic
  | Arith WBop
-- Flow Control
  | Label l
  | Call l
  | Branch WCond l
  | Return
  | End
-- Heap Access
  | Store
  | Retrieve
  deriving (Functor, Foldable, Traversable)

deriving instance Eq l => Eq (WInstruction l)
deriving instance Show l => Show (WInstruction l)