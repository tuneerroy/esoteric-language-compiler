module WhitespaceParser (tokenize, commandP, blockP) where

import Control.Applicative (Alternative (many, some))
import Data.Foldable (asum)
import Data.Functor (($>))
import Data.List (foldl', intercalate)
import Data.Maybe (mapMaybe)
import GHC.Base ((<|>))
import Parser ( Parser, token, tokens, parse )
import WhitespaceSyntax (WBop(..), WCond(..), WVal(..), WInstruction(..))
import Prelude hiding (filter)
import Control.Monad.State (StateT(StateT))

-- | For tests
-- | For tests
import Test.QuickCheck (Arbitrary(..), Gen(..), oneof, suchThat, listOf)
import Test.QuickCheck.Gen (elements)
import Control.Applicative (Applicative(liftA2))

data WToken = Space | Tab | LF deriving (Eq, Show, Ord)
type Command = WInstruction [WToken]
type WParser a = Parser WToken a

-- This could be done with a Read instance for Token (maybe change later)
parseToken :: Char -> Maybe WToken
parseToken ' ' = Just Space
parseToken '\t' = Just Tab
parseToken '\n' = Just LF
parseToken _ = Nothing

tokenize :: String -> [WToken]
tokenize = mapMaybe parseToken

constPTok :: WToken -> a -> WParser a
constPTok t = (token t $>)

constP :: [WToken] -> a -> WParser a
constP ts = (tokens ts $>)

numberP :: WParser WVal
numberP =
  (constPTok Space negate <|> constPTok Tab id)
    <*> (binToNum <$> some (constPTok Space 0 <|> constPTok Tab 1))
  where
    binToNum :: [WVal] -> WVal
    binToNum = foldl' (\acc x -> acc * 2 + x) 0

labelP :: WParser [WToken]
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
        *> (Arith <$> asum
          [ constP [Space, Space] Add,
            constP [Space, Tab] Sub,
            constP [Space, LF] Mul,
            constP [Tab, Space] Div,
            constP [Tab, Tab] Mod
          ])
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


-- | Tests

instance Arbitrary WToken where
  arbitrary :: Gen WToken
  arbitrary = elements [Space, Tab, LF]

commandToTokens :: Command -> [WToken]
commandToTokens = undefined

blockToTokens :: [Command] -> [WToken]
blockToTokens = intercalate [LF] . map commandToTokens

prop_roundtrip_tokens :: [Command] -> Bool
prop_roundtrip_tokens cs = case parse blockP $ blockToTokens cs of
  Nothing -> False
  Just parseResult -> cs == parseResult

tokenToChar :: WToken -> Char
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

