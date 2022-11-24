module WhitespaceParser where

import Control.Applicative (Alternative (many, some))
import Data.Foldable (asum)
import Data.Functor (($>))
import Data.List (foldl')
import Data.Maybe (mapMaybe)
import GHC.Base ((<|>))
import Parser ( Parser, token, tokens )
import WhitespaceSyntax (Block, Command (..), Label, Token (..), Val)
import Prelude hiding (filter)

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

numberP :: WParser Val
numberP =
  (constPTok Space negate <|> constPTok Tab id)
    <*> (binToNum <$> some (constPTok Space 0 <|> constPTok Tab 1))
  where
    binToNum :: [Int] -> Val
    binToNum = foldl' (\acc x -> acc * 2 + x) 0

labelP :: WParser Label
labelP = some (token Space <|> token Tab)

commandP :: WParser Command
commandP = asum [ioP, stackP, arithP, flowP, heapP] <* (token LF <|> pure [])
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
        *> asum
          [ constP [Space, Space] Add,
            constP [Space, Tab] Sub,
            constP [Space, LF] Mul,
            constP [Tab, Space] Div,
            constP [Tab, Tab] Mod
          ]
    flowP :: WParser Command
    flowP =
      token LF
        *> asum
          [ constP [Space, Space] Mark <*> labelP,
            constP [Space, Tab] CallSub <*> labelP,
            constP [Space, LF] Jump <*> labelP,
            constP [Tab, Space] BranchZ <*> labelP,
            constP [Tab, Tab] BranchN <*> labelP,
            constP [Tab, LF] EndSub,
            constP [LF, LF] EndProg
          ]
    heapP :: WParser Command
    heapP =
      tokens [Tab, Tab]
        *> constPTok Space Store
        <|> constPTok Tab Retrieve

blockP :: WParser Block
blockP = many commandP