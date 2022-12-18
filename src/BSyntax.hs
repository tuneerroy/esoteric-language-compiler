module BSyntax where

import Test.QuickCheck (Arbitrary (arbitrary, shrink), Gen)
import Test.QuickCheck.Gen (elements, oneof)

data BInstruction a
  = IncrPtr -- ptr++
  | DecrPtr -- ptr--
  | IncrByte -- (*ptr)++
  | DecrByte -- (*ptr)--
  | Output -- putchar(*ptr);
  | Input -- ptr = getchar();
  | WhileStart a -- while (*ptr) {
  | WhileEnd a -- }
  deriving (Eq, Show)

instance Arbitrary a => Arbitrary (BInstruction a) where
  arbitrary :: Arbitrary a => Gen (BInstruction a)
  arbitrary =
    oneof $
      ( pure
          <$> [ IncrPtr,
                DecrPtr,
                IncrByte,
                DecrByte,
                Output,
                Input
              ]
      )
        ++ fmap (<$> arbitrary) [WhileStart, WhileEnd]

  shrink :: BInstruction a -> [BInstruction a]
  shrink v = []