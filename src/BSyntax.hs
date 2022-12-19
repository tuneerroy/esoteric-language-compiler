module BSyntax where

import Control.Monad.State (MonadState (..))
import Control.Monad.State.Lazy (MonadTrans (..))
import Test.QuickCheck (Arbitrary (arbitrary, shrink), Gen)
import Test.QuickCheck.Gen (elements, listOf, oneof)

data BInstruction
  = IncrPtr -- ptr++
  | DecrPtr -- ptr--
  | IncrByte -- (*ptr)++
  | DecrByte -- (*ptr)--
  | Output -- putchar(*ptr);
  | Input -- ptr = getchar();
  | While [BInstruction] -- while (*ptr) { ... }
  deriving (Eq, Show)

instance Arbitrary BInstruction where
  arbitrary :: Gen BInstruction
  arbitrary = aux maxDepth
    where
      maxDepth :: Int
      maxDepth = 3

      aux :: Int -> Gen BInstruction
      aux 0 = simpleGen
      aux n = oneof [While <$> listOf (aux $ n - 1), simpleGen]

      simpleGen :: Gen BInstruction
      simpleGen = elements [IncrPtr, DecrPtr, IncrByte, DecrByte, Output, Input]

  shrink :: BInstruction -> [BInstruction]
  shrink (While b) = b
  shrink _ = []