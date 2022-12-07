module WParserTest where

import Parser (parse)
import Test.HUnit hiding (Label)
import Test.QuickCheck hiding (Discard)
import WParser (Token (..), WCommand, WLabel (..), wParseString, wParseTokens)
import WSyntax
  ( WBop (..),
    WCond (..),
    WInstruction (..),
  )
import Data.Foldable (Foldable(toList))
import Data.List.NonEmpty(NonEmpty(..))

instance Arbitrary Token where
  arbitrary :: Gen Token
  arbitrary = elements [Space, Tab, LF]

commandToTokens :: WCommand -> [Token]
commandToTokens c = case c of
  -- IO
  InputChar -> [Tab, LF, Tab, Space]
  InputNum -> [Tab, LF, Tab, Tab]
  OutputChar -> [Tab, LF, Space, Space]
  OutputNum -> [Tab, LF, Space, Tab]
  -- Stack
  Push n -> [Space, Space] ++ numToTokens n
  Dup -> [Space, LF, Space]
  Swap -> [Space, LF, Tab]
  Discard -> [Space, LF, LF]
  Copy n -> [Space, Tab, Space] ++ numToTokens n
  Slide n -> [Space, Tab, LF] ++ numToTokens n
  -- Arith
  Arith Add -> [Tab, Space, Space, Space]
  Arith Sub -> [Tab, Space, Space, Tab]
  Arith Mul -> [Tab, Space, Space, LF]
  Arith Div -> [Tab, Space, Tab, Space]
  Arith Mod -> [Tab, Space, Tab, Tab]
  -- Flow
  Label (WLabel l) -> [LF, Space, Space] ++ toList l ++ [LF]
  Call (WLabel l) -> [LF, Space, Tab] ++ toList l ++ [LF]
  Branch Any (WLabel l) -> [LF, Space, LF] ++ toList l ++ [LF]
  Branch Zero (WLabel l) -> [LF, Tab, Space] ++ toList l ++ [LF]
  Branch Neg (WLabel l) -> [LF, Tab, Tab] ++ toList l ++ [LF]
  Return -> [LF, Tab, LF]
  End -> [LF, LF, LF]
  -- Heap
  Store -> [Tab, Tab, Space]
  Retrieve -> [Tab, Tab, Tab]

numToTokens :: Int -> [Token]
numToTokens 0 = [Space, Space, LF]
numToTokens n =
  if n < 0
    then Tab : posNumToTokens (-1 * n) ++ [LF]
    else Space : posNumToTokens n ++ [LF]
  where
    posNumToTokens 0 = []
    posNumToTokens n
      | even n = posNumToTokens (n `div` 2) ++ [Space]
      | otherwise = posNumToTokens (n `div` 2) ++ [Tab]

blockToTokens :: [WCommand] -> [Token]
blockToTokens = concatMap commandToTokens

prop_roundtrip_tokens :: [WCommand] -> Bool
prop_roundtrip_tokens cs = case wParseTokens $ blockToTokens cs of
  Nothing -> False
  Just parseResult -> cs == parseResult

tokenToChar :: Token -> Char
tokenToChar Space = ' '
tokenToChar Tab = '\t'
tokenToChar LF = '\n'

newtype WProgramString = WP String deriving (Show)

instance Arbitrary WLabel where
  arbitrary :: Gen WLabel
  arbitrary = WLabel <$> ((:|) <$> nonLF <*> listOf nonLF)
    where nonLF :: Gen Token = oneof [return Tab, return Space]

  shrink :: WLabel -> [WLabel]
  shrink = const []

instance Arbitrary WProgramString where
  arbitrary :: Gen WProgramString
  arbitrary = WP <$> (arbitrary >>= mixGarbage . map tokenToChar . blockToTokens)
    where
      garbageChar :: Gen Char
      garbageChar = suchThat arbitrary (`notElem` [' ', '\n', '\t'])

      mixGarbage :: String -> Gen String
      mixGarbage [] = listOf garbageChar
      mixGarbage s@(c : cs) =
        oneof [(c :) <$> mixGarbage cs, (:) <$> garbageChar <*> mixGarbage s]

prop_program_parse :: WProgramString -> Bool
prop_program_parse (WP s) = case wParseString s of
  Nothing -> False
  Just _ -> True

qc :: IO ()
qc = do
  putStrLn "roundtrip_tokens"
  quickCheck prop_roundtrip_tokens
  putStrLn "program_parse"
  quickCheck prop_program_parse
