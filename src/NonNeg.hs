module NonNeg (NonNeg) where
import Test.QuickCheck (Arbitrary (arbitrary, shrink))
import Test.QuickCheck.Gen(Gen)

newtype NonNeg = NonNeg Int deriving (Eq, Show, Ord)

instance Enum NonNeg where
  toEnum :: Int -> NonNeg
  toEnum = NonNeg . max 0
  fromEnum :: NonNeg -> Int
  fromEnum (NonNeg x) = x

instance Arbitrary NonNeg where
  arbitrary :: Gen NonNeg
  arbitrary = toEnum <$> arbitrary
  shrink :: NonNeg -> [NonNeg]
  shrink (NonNeg x) = map toEnum $ shrink x
