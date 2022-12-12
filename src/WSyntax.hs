module WSyntax (WBop (..), WCond (..), WVal, WInstruction (..)) where

-- import Program (Instruction (..))
import Test.QuickCheck (Arbitrary (arbitrary, shrink), Gen)
import Test.QuickCheck.Gen (elements, oneof)

data WBop = Add | Sub | Mul | Div | Mod deriving (Eq, Show)

data WCond = Any | Zero | Neg deriving (Eq, Show)

-- In the offical implementation, stack values are Integer
-- But that would be a pain to convert assembly to
type WVal = Int

data WInstruction l
  = -- IO
    InputChar
  | InputNum
  | OutputChar
  | OutputNum
  | -- Stack
    Push WVal
  | Dup
  | Swap
  | Discard
  | Copy WVal
  | Slide WVal
  | -- Arithmetic
    Arith WBop
  | -- Flow Control
    Label l
  | Call l
  | Branch WCond l
  | Return
  | End
  | -- Heap Access
    Store
  | Retrieve
  deriving (Functor, Foldable, Traversable)

deriving instance Eq l => Eq (WInstruction l)

deriving instance Show l => Show (WInstruction l)

instance Instruction WInstruction where
  extractLabel :: WInstruction l -> Maybe l
  extractLabel (Label l) = Just l
  extractLabel _ = Nothing

instance Arbitrary l => Arbitrary (WInstruction l) where
  arbitrary :: Arbitrary l => Gen (WInstruction l)
  arbitrary =
    oneof
      [ pure InputChar,
        pure InputNum,
        pure OutputChar,
        pure OutputNum,
        Push <$> arbitrary,
        pure Dup,
        pure Swap,
        pure Discard,
        Copy <$> arbitrary,
        Slide <$> arbitrary,
        Arith <$> elements [Add, Sub, Mul, Div, Mod],
        Label <$> arbitrary,
        Call <$> arbitrary,
        Branch <$> elements [Any, Zero, Neg] <*> arbitrary,
        pure Return,
        pure End,
        pure Store,
        pure Retrieve
      ]

  shrink :: WInstruction l -> [WInstruction l]
  shrink v = []