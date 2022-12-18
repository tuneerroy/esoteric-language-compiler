module BParser where

import BSyntax
import Control.Applicative (Alternative (many))
import Data.Foldable (asum)
import Data.Functor (($>))
import Data.Maybe (mapMaybe)
import Parser (Parser, parse, parseFromFile, token, tokens)
import WSyntax (WInstruction)

data Token
  = Geq
  | Leq
  | Plus
  | Minus
  | Period
  | Comma
  | LBracket
  | RBracket
  deriving (Eq, Show)

type BParser a = Parser Token a

parseToken :: Char -> Maybe Token
parseToken '>' = Just Geq
parseToken '<' = Just Leq
parseToken '+' = Just Plus
parseToken '-' = Just Minus
parseToken '.' = Just Period
parseToken ',' = Just Comma
parseToken '[' = Just LBracket
parseToken ']' = Just RBracket
parseToken _ = Nothing

-- TODO: SHOULD EXTRACT THIS TO Parser.hs, bit repeated
tokenize :: String -> [Token]
tokenize = mapMaybe parseToken

constPTok :: Token -> a -> BParser a
constPTok t = (token t $>)

commandB :: BParser (BInstruction ())
commandB =
  asum $
    uncurry constPTok
      <$> [ (Geq, IncrPtr),
            (Leq, DecrPtr),
            (Plus, IncrByte),
            (Minus, DecrByte),
            (Period, Output),
            (Comma, Input),
            (LBracket, WhileStart ()),
            (RBracket, WhileEnd ())
          ]

blockB :: BParser [BInstruction ()]
blockB = many commandB

bParseString :: String -> Maybe [BInstruction ()]
bParseString = parse blockB . tokenize