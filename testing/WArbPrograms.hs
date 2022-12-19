-- | Arbitrary instances for different complexities/validities of programs
module WArbPrograms where

import Control.Monad.State.Lazy (State, MonadState (..), evalState)
import Test.QuickCheck (Arbitrary (..), Property)
import Test.QuickCheck qualified as QC
import Test.QuickCheck.Arbitrary (Arbitrary)
import Test.QuickCheck.Gen (Gen)
import WSyntax (WInstruction (..))

-- | Quickcheck tests
numPops :: WInstruction l -> Int
numPops InputChar = 0
numPops InputNum = 0
numPops OutputChar = 1
numPops OutputNum = 1
numPops (Push _) = 0
numPops Dup = 1
numPops Swap = 2
numPops Discard = 1
numPops (Copy n) = fromEnum n + 1
numPops (Slide n) = fromEnum n + 1
numPops (Arith _) = 2
numPops (Label _) = 0
numPops (Call _) = 0
numPops (Branch _ _) = 0
numPops Return = 0
numPops End = 0
numPops Store = 1
numPops Retrieve = 0

numPushes :: WInstruction l -> Int
numPushes InputChar = 1
numPushes InputNum = 1
numPushes OutputChar = 0
numPushes OutputNum = 0
numPushes (Push _) = 1
numPushes Dup = 2
numPushes Swap = 2
numPushes Discard = 0
numPushes (Copy n) = fromEnum n + 2
numPushes (Slide n) = 1
numPushes (Arith _) = 1
numPushes (Label _) = 0
numPushes (Call _) = 0
numPushes (Branch _ _) = 0
numPushes Return = 0
numPushes End = 0
numPushes Store = 0
numPushes Retrieve = 1

class InstructionSet a where
    unpack :: a -> WInstruction Int

-- | Instructions that modify the stack, and do nothing else
newtype StackInstr = StackInstr (WInstruction Int)
  deriving (Eq, Show)

instance InstructionSet StackInstr where
    unpack :: StackInstr -> WInstruction Int
    unpack (StackInstr instr) = instr

instance Arbitrary StackInstr where
  arbitrary :: Gen StackInstr
  arbitrary =
    StackInstr
      <$> QC.oneof
        [ Push <$> arbitrary,
          pure Dup,
          pure Swap,
          pure Discard,
          Copy <$> arbitrary,
          Slide <$> arbitrary,
          Arith <$> arbitrary
        ]
  shrink :: StackInstr -> [StackInstr]
  shrink = const []

-- | Verify that a program should not throw a stack empty error
stackVerify :: [WInstruction l] -> Bool
stackVerify = aux 0
  where
    aux :: Int -> [WInstruction l] -> Bool
    aux _ [] = True
    aux stackHeight (x : xs) = do
      let stackHeight' = stackHeight - numPops x
      stackHeight' >= 0 && aux (stackHeight' + numPushes x) xs

-- | A program that has only stack modification instructions
newtype StackProg = StackProg [WInstruction Int] deriving (Show)

instance Arbitrary StackProg where
    arbitrary :: Gen StackProg
    arbitrary = do
        instructionList <- map (\(StackInstr instr) -> instr)
            <$> QC.listOf (arbitrary :: Gen StackInstr)
        return (StackProg (instructionList ++ [End]))

-- | A program with only stack modification instructions that doesn't throw
-- | any empty stack errors
newtype NEStackProg = NEStackProg [WInstruction Int] deriving (Show)

stackValidate :: [WInstruction l] -> [WInstruction l]
stackValidate l = evalState (mAvoidEmptyStack l) 0
  where
    mAvoidEmptyStack :: [WInstruction l] -> State Int [WInstruction l]
    mAvoidEmptyStack (x : xs) = do
      stackHeight <- get
      if numPops x <= stackHeight
        then do
          put (stackHeight - numPops x + numPushes x)
          (x :) <$> mAvoidEmptyStack xs
        else mAvoidEmptyStack xs
    mAvoidEmptyStack [] = return []

shrinkList :: Arbitrary a => [a] -> [[a]]
shrinkList = shrink

instance Arbitrary NEStackProg where
  arbitrary :: Gen NEStackProg
  arbitrary = do
    instructionList <- QC.listOf simpleInstruction
    return $ NEStackProg (stackValidate instructionList ++ [End])
    where
      simpleInstruction =
        QC.frequency
          [ (3, Push <$> arbitrary),
            (1, pure Dup),
            (1, pure Swap),
            (1, pure Discard),
            (1, Copy <$> genSmallNonNeg),
            (1, Slide <$> genSmallNonNeg),
            (1, Arith <$> arbitrary)
          ]
        where
          genSmallNonNeg = toEnum <$> QC.chooseInt (0, 5)

      shrink (NEStackProg instrs) =
        NEStackProg . (++ [End]) <$> shrinkList (init instrs)