module BParser where

import BSyntax (BInstruction (..))
import Control.Applicative (Alternative (many))
import Data.Foldable (asum)
import Data.Functor (($>))
import Data.Maybe (mapMaybe)
import Parser (Parser, constPTok, eof, parse, parseFromFile, token, tokens)

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

command :: BParser BInstruction
command =
  asum $
    whileParser :
    ( uncurry constPTok
        <$> [ (Geq, IncrPtr),
              (Leq, DecrPtr),
              (Plus, IncrByte),
              (Minus, DecrByte),
              (Period, Output),
              (Comma, Input)
            ]
    )
  where
    whileParser :: BParser BInstruction
    whileParser = While <$> (token LBracket *> many command <* token RBracket)

block :: BParser [BInstruction]
block = many command <* eof

parseTokens :: [Token] -> Maybe [BInstruction]
parseTokens = parse block

parseString :: String -> Maybe [BInstruction]
parseString = parseTokens . tokenize