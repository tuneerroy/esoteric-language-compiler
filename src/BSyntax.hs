module BSyntax where

import Test.QuickCheck (Arbitrary (arbitrary, shrink), Gen)
import Test.QuickCheck.Gen (elements, oneof)

data BInstruction
  = IncrPtr -- ptr++
  | DecrPtr -- ptr--
  | IncrByte -- (*ptr)++
  | DecrByte -- (*ptr)--
  | Output -- putchar(*ptr);
  | Input -- ptr = getchar();
  | While [BInstruction]
  --   | WhileStart a -- while (*ptr) {
  --   | WhileEnd a -- }
  deriving (Eq, Show)

instance Arbitrary BInstruction where
  arbitrary :: Gen BInstruction
  arbitrary =
    oneof $
      (While <$> arbitrary) :
      ( pure
          <$> [ IncrPtr,
                DecrPtr,
                IncrByte,
                DecrByte,
                Output,
                Input
              ]
      )

  shrink :: BInstruction -> [BInstruction]
  shrink v = []