module NonNeg (NonNeg) where

import Test.QuickCheck (Arbitrary (arbitrary, shrink))
import Test.QuickCheck.Gen (Gen)

newtype NonNeg = NonNeg Int deriving (Eq, Show, Ord)

-- | The Whitespace language treats negative numbers for slide and copy as 0
-- | Therefore we expose toEnum that clamps to 0 as the only way to use this type
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
