-- | Arbitrary instances for different complexities/validities of programs
module WArbPrograms where

import Control.Applicative (Applicative (liftA2))
import Control.Monad (guard)
import Control.Monad.State.Lazy (MonadState (..), State, evalState)
import NonNeg (NonNeg)
import Test.QuickCheck (Arbitrary (..), Property)
import Test.QuickCheck qualified as QC
import Test.QuickCheck.Arbitrary (Arbitrary)
import Test.QuickCheck.Gen (Gen)
import WSyntax (WBop (..), WInstruction (..))

-- | Number of times the instruction pops the stack
numPops :: WInstruction l -> Int
numPops InputChar = 1
numPops InputNum = 1
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
numPops (Jump _) = 0
numPops (Branch _ _) = 0
numPops Return = 0
numPops End = 0
numPops Store = 2
numPops Retrieve = 1

-- | Number of times the instruction pushes to the stack
numPushes :: WInstruction l -> Int
numPushes InputChar = 0
numPushes InputNum = 0
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
numPushes (Jump _) = 0
numPushes (Branch _ _) = 0
numPushes Return = 0
numPushes End = 0
numPushes Store = 0
numPushes Retrieve = 1

-- | Instructions that modify the stack and nothing else
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

-- | Instructions that modify the stack, but in a more reasonable range
smallStackInstr :: Gen (WInstruction l)
smallStackInstr = do
  instr <- arbStackInstr
  case instr of
    Copy n -> return (Copy $ toEnum (fromEnum n `mod` 5))
    Slide n -> return (Slide $ toEnum (fromEnum n `mod` 5))
    _ -> return instr

-- | Creates a list of WInstructions and places and end on it
programOf :: Gen (WInstruction l) -> Gen [WInstruction l]
programOf gen = do
  size <- QC.getSize
  -- We change the size parameter because the default size is small
  instrs <- QC.resize (size * 5) $ QC.listOf gen
  return (instrs ++ [End])

-- | Program that only modifies stack instrucionts
stackProgram :: Gen [WInstruction l]
stackProgram = programOf smallStackInstr

-- | Returns nothing if the program would yield an empty stack pop
-- Otherwise retunrs the final height of the stack
stackVerify :: [WInstruction l] -> Maybe Int
stackVerify = aux 0
  where
    aux :: Int -> [WInstruction l] -> Maybe Int
    aux stackHeight [] = return stackHeight
    aux stackHeight (x : xs) = do
      let stackHeight' = stackHeight - numPops x
      guard (stackHeight' >= 0)
      aux (stackHeight' + numPushes x) xs

-- | Erases instructions that would pop an empty stack
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

-- | A program that only modifies the stack and is valid
validStackProgram :: Gen [WInstruction l]
validStackProgram =
  stackValidate
    <$> programOf (QC.oneof [smallStackInstr, Push <$> arbitrary])

outputInstr :: Gen (WInstruction l)
outputInstr = QC.elements [OutputNum]

-- | Program that does not pop an empty stack and also outputs values
validOutputProgram :: Gen [WInstruction l]
validOutputProgram =
  stackValidate
    <$> programOf
      ( QC.frequency
          [ (5, Push <$> arbitrary),
            (1, smallStackInstr),
            (2, outputInstr)
          ]
      )

-- | Instructions that take input
inputInstr :: Gen (WInstruction l)
inputInstr = QC.elements [InputChar, InputNum]

-- | Program that takes inputs
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

-- | Adds in a store and a subsequent load to the same address sandwiched between
-- other instructions given
sprinkleHeap :: Gen (WInstruction l) -> Gen [WInstruction l]
sprinkleHeap gen = do
  begin <- QC.listOf gen
  (addr :: Int) <- abs <$> arbitrary
  (val :: Int) <- abs <$> arbitrary
  let storeInstrs = [Push addr, Push val, Store]
  middle <- QC.listOf gen
  let loadInstrs = [Push addr, Retrieve]
  end <- QC.listOf gen
  return $ begin <> storeInstrs <> middle <> loadInstrs <> end

-- | Instructions that have heap
heapInstr :: Gen (WInstruction l)
heapInstr = QC.elements [Store, Retrieve]

-- | A program that can both modify the heap and output characters that does not
-- pop the empty stack
validHeapAndOutputProgram :: Gen [WInstruction l]
validHeapAndOutputProgram = (<> [End]) . stackValidate <$> sprinkleHeap gen
  where
    gen :: Gen (WInstruction l)
    gen =
      QC.frequency
        [ (5, Push . abs <$> arbitrary),
          (1, smallPosStackInstr),
          (2, pure OutputNum),
          (1, heapInstr)
        ]

    smallPosStackInstr :: Gen (WInstruction l)
    smallPosStackInstr = do
      instr <- smallStackInstr
      return $ case instr of
        Push n -> Push $ abs n
        _ -> instr

-- Check a property intelligently for a whitespace program
checkProp :: (Show l, Arbitrary l) => Gen [WInstruction l] -> ([WInstruction l] -> Property) -> IO ()
checkProp gen prop = QC.quickCheck (QC.forAllShrink gen (map (\l' -> l' <> [End]) . shrink . init) prop)
