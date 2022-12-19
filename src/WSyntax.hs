module WSyntax (WBop (..), WCond (..), WInstruction (..)) where

-- import Program (Instruction (..))

import NonNeg (NonNeg)
import Test.QuickCheck (Arbitrary (arbitrary, shrink), Gen)
import Test.QuickCheck.Gen (elements, oneof)

data WBop = Add | Sub | Mul | Div | Mod deriving (Eq, Show)

instance Arbitrary WBop where
  arbitrary = elements [Add, Sub, Mul, Div, Mod]

data WCond = Zero | Neg deriving (Eq, Show)

-- In the offical implementation, stack values are Integer
-- But that would be a pain to convert assembly to

data WInstruction l
  = -- IO
    InputChar
  | InputNum
  | OutputChar
  | OutputNum
  | -- Stack
    Push Int
  | Dup
  | Swap
  | Discard
  | Copy NonNeg
  | Slide NonNeg
  | -- Arithmetic
    Arith WBop
  | -- Flow Control
    Label l
  | Call l
  | Branch WCond l
  | Jump l
  | Return
  | End
  | -- Heap Access
    Store
  | Retrieve
  deriving (Functor, Foldable, Traversable)

deriving instance Eq l => Eq (WInstruction l)

deriving instance Show l => Show (WInstruction l)

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
        Arith <$> arbitrary,
        Label <$> arbitrary,
        Call <$> arbitrary,
        Branch <$> elements [Zero, Neg] <*> arbitrary,
        Jump <$> arbitrary,
        pure Return,
        pure End,
        pure Store,
        pure Retrieve
      ]

  shrink :: WInstruction l -> [WInstruction l]
  shrink v = []