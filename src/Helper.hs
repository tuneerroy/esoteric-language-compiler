module Helper where

import Control.Applicative (Alternative (many, some, (<|>)), Applicative (liftA2))
import Control.Monad.Trans (MonadTrans (..))
import Data.Foldable (asum)
import Data.Functor (($>))
import Data.List (foldl', intercalate)
import Data.List.NonEmpty (NonEmpty, nonEmpty)
import Data.Maybe (mapMaybe)
import Parser (Parser, parse, parseFromFile, token, tokens)
import WSyntax (WBop (..), WCond (..), WInstruction (..))

data Token = Space | Tab | LF deriving (Eq, Show, Ord)

newtype WLabel = WLabel (NonEmpty Token) deriving (Eq, Show)

type WCommand = WInstruction WLabel

type WParser a = Parser Token a

tokenize :: String -> [Token]
tokenize ('[' : 'S' : 'p' : 'a' : 'c' : 'e' : ']' : s) = Space : tokenize s
tokenize ('[' : 'T' : 'a' : 'b' : ']' : s) = Tab : tokenize s
tokenize ('[' : 'L' : 'F' : ']' : s) = LF : tokenize s
tokenize (c : cs) = tokenize cs
tokenize [] = []

tokenToChar :: Token -> Char
tokenToChar Space = ' '
tokenToChar Tab = '\t'
tokenToChar LF = '\n'

compile :: String -> String
compile s = map tokenToChar $ tokenize s

main :: IO ()
main = do
  putStrLn "Input filename: "
  filename <- getLine
  program <- readFile filename
  writeFile "res.txt" $ compile program

-- >>> compile "[Space][Space][Space][LF][Space][LF][Space][Tab][LF][Tab][Tab][Tab][Tab][Tab][LF][Tab][Space][Space][LF][LF][Space][Space][Tab][LF][Space][Space][Space][Tab][LF][Tab][LF][Space][Tab][LF][Space][LF][Tab][LF][LF][Space][Space][Space][LF][Space][Space][Space][LF][Tab][LF][Space][Tab][LF][LF][LF]"
-- "   \n \n \t\n\t\t\t\t\t\n\t  \n\n  \t\n   \t\n\t\n \t\n \n\t\n\n   \n   \n\t\n \t\n\n\n"

constPTok :: Token -> a -> WParser a
constPTok t = (token t $>)

constP :: [Token] -> a -> WParser a
constP ts = (tokens ts $>)

numberP :: WParser Int
numberP =
  (constPTok Tab negate <|> constPTok Space id)
    <*> (binToNum <$> (some (constPTok Space 0 <|> constPTok Tab 1) <* token LF))
  where
    binToNum :: [Int] -> Int
    binToNum = foldl' (\acc x -> acc * 2 + x) 0

labelP :: WParser WLabel
labelP = WLabel <$> (some (token Space <|> token Tab) <* token LF >>= lift . nonEmpty)

commandP :: WParser WCommand
commandP = asum [ioP, stackP, arithP, flowP, heapP]
  where
    ioP :: WParser WCommand
    ioP =
      tokens [Tab, LF]
        *> asum
          [ constP [Tab, Space] InputChar,
            constP [Tab, Tab] InputNum,
            constP [Space, Space] OutputChar,
            constP [Space, Tab] OutputNum
          ]
    stackP :: WParser WCommand
    stackP =
      token Space
        *> asum
          [ constPTok Space Push <*> numberP,
            constP [LF, Space] Dup,
            constP [Tab, Space] (Copy . toEnum) <*> numberP,
            constP [LF, Tab] Swap,
            constP [LF, LF] Discard,
            constP [Tab, LF] (Slide . toEnum) <*> numberP
          ]
    arithP :: WParser WCommand
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
    flowP :: WParser WCommand
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
    heapP :: WParser WCommand
    heapP =
      tokens [Tab, Tab]
        *> ( constPTok Space Store
               <|> constPTok Tab Retrieve
           )

blockP :: WParser [WCommand]
-- blockP = many (commandP <* token LF <|> commandP)
blockP = many commandP

wParseTokens :: [Token] -> Maybe [WCommand]
wParseTokens = parse blockP

wParseString :: String -> Maybe [WCommand]
wParseString = wParseTokens . tokenize
