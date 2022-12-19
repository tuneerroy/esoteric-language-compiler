-- | Arbitrary instances for different complexities/validities of programs
module WArbPrograms where

import Control.Monad.State.Lazy (MonadState (..), State, evalState)
import NonNeg (NonNeg)
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

arbStackInstr :: Gen (WInstruction l)
arbStackInstr =
  QC.oneof
    [ Push <$> arbitrary,
      pure Dup,
      pure Swap,
      pure Discard,
      Copy <$> arbitrary,
      Slide <$> arbitrary,
      Arith <$> arbitrary
    ]

smallStackInstr :: Gen (WInstruction l)
smallStackInstr = do
  instr <- arbStackInstr
  case instr of
    Copy n -> return (Copy $ toEnum (fromEnum n `mod` 5))
    Slide n -> return (Slide $ toEnum (fromEnum n `mod` 5))
    _ -> return instr

programOf :: Gen (WInstruction l) -> Gen [WInstruction l]
programOf gen = (++ [End]) <$> QC.listOf gen

stackProgram :: Gen [WInstruction l]
stackProgram = programOf smallStackInstr

stackVerify :: [WInstruction l] -> Bool
stackVerify = aux 0
  where
    aux :: Int -> [WInstruction l] -> Bool
    aux _ [] = True
    aux stackHeight (x : xs) = do
      let stackHeight' = stackHeight - numPops x
      stackHeight' >= 0 && aux (stackHeight' + numPushes x) xs

stackValidate :: [WInstruction l] -> [WInstruction l]
stackValidate l = evalState (mStackValidate l) 0
  where
    mStackValidate :: [WInstruction l] -> State Int [WInstruction l]
    mStackValidate (x : xs) = do
      stackHeight <- get
      if numPops x <= stackHeight
        then do
          put (stackHeight - numPops x + numPushes x)
          (x :) <$> mStackValidate xs
        else mStackValidate xs
    mStackValidate [] = return []

validStackProgram :: Gen [WInstruction l]
validStackProgram =
  stackValidate
    <$> programOf (QC.oneof [smallStackInstr, Push <$> arbitrary])

outputInstr :: Gen (WInstruction l)
outputInstr = QC.elements [OutputChar, OutputNum]

validOutputProgram :: Gen [WInstruction l]
validOutputProgram =
  stackValidate
    <$> programOf
      ( QC.frequency
          [ (5, Push <$> arbitrary),
            (1, smallStackInstr),
            (3, outputInstr)
          ]
      )

inputInstr :: Gen (WInstruction l)
inputInstr = QC.elements [InputChar, InputNum]

validInputProgram :: Gen [WInstruction l]
validInputProgram =
  stackValidate
    <$> programOf
      ( QC.frequency
          [ (5, Push <$> arbitrary),
            (1, smallStackInstr),
            (3, inputInstr)
          ]
      )