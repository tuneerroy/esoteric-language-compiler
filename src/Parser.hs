{-# LANGUAGE LambdaCase #-}

module Parser where

import Control.Applicative
import Control.Monad (guard)
import Control.Monad.Cont (MonadTrans (lift))
import Control.Monad.State (StateT (StateT), evalStateT, lift, runStateT)
import Data.Functor (($>))
import Data.Maybe (mapMaybe)
import System.IO qualified as IO
import System.IO.Error qualified as IO
import Prelude hiding (filter)

-- | Test modules

-- | Definition of parser type.
-- The same as from the Lu assignment, but generalized over any string of tokens
type Parser t a = StateT [t] Maybe a

-- | Returns the next character from the input
getTok :: Parser t t
getTok = StateT $ \case
  (c : cs) -> Just (c, cs)
  [] -> Nothing

-- | Parser that only succeeds as the end of input
eof :: Parser t ()
eof = StateT $ \case
  [] -> Just ((), [])
  _ -> Nothing

-- | Filter the results by a predicate
filter :: (a -> Bool) -> Parser t a -> Parser t a
filter f p = StateT $ \s -> do
  (c, cs) <- runStateT p s
  guard (f c)
  return (c, cs)

-- | Returns the next character if it satisfies a given predicate
satisfy :: (t -> Bool) -> Parser t t
satisfy p = filter p getTok

-- | Parses and returns the specific character
token :: Eq t => t -> Parser t t
token t = satisfy (t ==)

-- | Parses and returns the specified string
tokens :: Eq t => [t] -> Parser t [t]
tokens = foldr (\t p -> (:) <$> token t <*> p) (pure [])

-- | Consumes a token, and outputs a constant value
constPTok :: Eq t => t -> a -> Parser t a
constPTok t = (token t $>)

-- | Consumes a string of tokens, and outputs a constant value
constP :: Eq t => [t] -> a -> Parser t a
constP ts = (tokens ts $>)

-- | Run the parser
parse :: Parser t a -> [t] -> Maybe a
parse = evalStateT

-- | Run the parser, outputing an either
parseToEither :: Parser t a -> [t] -> Either String a
parseToEither p s = case parse p s of
  Just x -> Right x
  Nothing -> Left "No parses"

-- | Run the parser on a file input
parseFromFile :: Parser Char a -> FilePath -> IO (Either String a)
parseFromFile parser filename = do
  IO.catchIOError
    ( do
        handle <- IO.openFile filename IO.ReadMode
        str <- IO.hGetContents handle
        pure $ parseToEither parser str
    )
    (return . Left . show)