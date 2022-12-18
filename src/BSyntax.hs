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
  | WhileStart -- while (*ptr) {
  | WhileEnd -- }
  deriving (Eq, Show)

instance Arbitrary BInstruction where
  arbitrary :: Gen BInstruction
  arbitrary =
    oneof $
      pure
        <$> [ IncrPtr,
              DecrPtr,
              IncrByte,
              DecrByte,
              Output,
              Input,
              WhileStart,
              WhileEnd
            ]

  shrink :: BInstruction -> [BInstruction]
  shrink v = []