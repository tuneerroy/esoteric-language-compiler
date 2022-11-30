module WhitespaceParser (wParse) where

-- For tests

import Control.Applicative (Alternative (many, some), Applicative (liftA2))
import Control.Monad.State (StateT (StateT))
import Data.Foldable (asum)
import Data.Functor (($>))
import Data.List (foldl', intercalate)
import Data.Maybe (mapMaybe)
import GHC.Base ((<|>))
import Parser (Parser, parse, token, tokens)
import Test.QuickCheck (Arbitrary (..), Gen (..), listOf, oneof, suchThat)
import Test.QuickCheck.Gen (elements)
import WhitespaceSyntax (WBop (..), WCond (..), WInstruction (..), WVal (..))
import Prelude hiding (filter)

data Token = Space | Tab | LF deriving (Eq, Show, Ord)

type Command = WInstruction [Token]

type WParser a = Parser Token a

-- This could be done with a Read instance for Token (maybe change later)
parseToken :: Char -> Maybe Token
parseToken ' ' = Just Space
parseToken '\t' = Just Tab
parseToken '\n' = Just LF
parseToken _ = Nothing

tokenize :: String -> [Token]
tokenize = mapMaybe parseToken

constPTok :: Token -> a -> WParser a
constPTok t = (token t $>)

constP :: [Token] -> a -> WParser a
constP ts = (tokens ts $>)

numberP :: WParser WVal
numberP =
  (constPTok Space negate <|> constPTok Tab id)
    <*> (binToNum <$> some (constPTok Space 0 <|> constPTok Tab 1))
  where
    binToNum :: [WVal] -> WVal
    binToNum = foldl' (\acc x -> acc * 2 + x) 0

labelP :: WParser [Token]
labelP = some (token Space <|> token Tab)

commandP :: WParser Command
commandP = asum [ioP, stackP, arithP, flowP, heapP]
  where
    ioP :: WParser Command
    ioP =
      tokens [Tab, Space]
        *> asum
          [ constP [Tab, Space] InputChar,
            constP [Tab, Tab] InputNum,
            constP [Space, Space] OutputChar,
            constP [Space, Tab] OutputNum
          ]
    stackP :: WParser Command
    stackP =
      token Space
        *> asum
          [ constPTok Space Push <*> numberP,
            constP [LF, Space] Dup,
            constP [LF, Tab] Swap,
            constP [LF, LF] Discard
          ]
    arithP :: WParser Command
    arithP =
      tokens [Tab, Space]
        *> ( Arith
               <$> asum
                 [ constP [Space, Space] Add,
                   constP [Space, Tab] Sub,
                   constP [Space, LF] Mul,
                   constP [Tab, Space] Div,
                   constP [Tab, Tab] Mod
                 ]
           )
    flowP :: WParser Command
    flowP =
      token LF
        *> asum
          [ constP [Space, Space] Label <*> labelP,
            constP [Space, Tab] Call <*> labelP,
            constP [Space, LF] (Branch Any) <*> labelP,
            constP [Tab, Space] (Branch Zero) <*> labelP,
            constP [Tab, Tab] (Branch Neg) <*> labelP,
            constP [Tab, LF] Return,
            constP [LF, LF] End
          ]
    heapP :: WParser Command
    heapP =
      tokens [Tab, Tab]
        *> constPTok Space Store
        <|> constPTok Tab Retrieve

blockP :: WParser [Command]
blockP = many (commandP <* token LF <|> commandP)

wParse :: String -> Maybe [Command]
wParse = parse blockP . tokenize

-- | Tests
instance Arbitrary Token where
  arbitrary :: Gen Token
  arbitrary = elements [Space, Tab, LF]

commandToTokens :: Command -> [Token]
commandToTokens = undefined

blockToTokens :: [Command] -> [Token]
blockToTokens = intercalate [LF] . map commandToTokens

prop_roundtrip_tokens :: [Command] -> Bool
prop_roundtrip_tokens cs = case parse blockP $ blockToTokens cs of
  Nothing -> False
  Just parseResult -> cs == parseResult

tokenToChar :: Token -> Char
tokenToChar Space = ' '
tokenToChar Tab = '\t'
tokenToChar LF = '\n'

newtype WProgramString = WP String

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
prop_program_parse (WP s) = case parse blockP $ tokenize s of
  Nothing -> False
  Just _ -> True
