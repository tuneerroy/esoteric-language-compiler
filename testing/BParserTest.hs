module BParserTest where

import BParser (Token (..), parseString, parseTokens)
import BSyntax (BInstruction (..))
import Data.Foldable (Foldable (toList))
import Data.Function ((&))
import Data.List.NonEmpty (NonEmpty (..))
import Parser (parse)
import Program (mkProgram)
import Test.HUnit ()
import Test.QuickCheck
  ( Arbitrary (..),
    Gen,
    elements,
    listOf,
    oneof,
    quickCheck,
    suchThat,
  )

instance Arbitrary Token where
  arbitrary :: Gen Token
  arbitrary = elements [Geq, Leq, Plus, Minus, Period, Comma, LBracket, RBracket]

instructionToTokens :: BInstruction -> [Token]
instructionToTokens instr = case instr of
  IncrPtr -> [Geq]
  DecrPtr -> [Leq]
  IncrByte -> [Plus]
  DecrByte -> [Minus]
  Output -> [Period]
  Input -> [Comma]
  While b -> LBracket : foldMap instructionToTokens b <> [RBracket]

prop_roundtrip_tokens :: [BInstruction] -> Bool
prop_roundtrip_tokens cs = case parseTokens (concatMap instructionToTokens cs) of
  Nothing -> False
  Just parseResult -> cs == parseResult

tokenToChar :: Token -> Char
tokenToChar Geq = '>'
tokenToChar Leq = '<'
tokenToChar Plus = '+'
tokenToChar Minus = '-'
tokenToChar Period = '.'
tokenToChar Comma = ','
tokenToChar LBracket = '['
tokenToChar RBracket = ']'

newtype BProgramString = BP String deriving (Show)

instance Arbitrary BProgramString where
  arbitrary :: Gen BProgramString
  arbitrary =
    BP <$> do
      t <- (arbitrary :: Gen [BInstruction])
      return $ concatMap instructionToTokens t & map tokenToChar

-- z = do
--   t <- (arbitrary :: Gen [BInstruction])
--   return $ concatMap instructionToTokens t & map tokenToChar

prop_program_parse :: BProgramString -> Bool
prop_program_parse (BP s) = not . null $ parseString s

qc :: IO ()
qc = do
  putStrLn "bparser_roundtrip_tokens"
  quickCheck prop_roundtrip_tokens
  putStrLn "bparser_program_parse"
  quickCheck prop_program_parse
