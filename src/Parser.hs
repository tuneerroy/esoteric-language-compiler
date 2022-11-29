{-# LANGUAGE LambdaCase #-}

module Parser where

import Control.Monad (guard)
import Control.Monad.State (StateT (StateT), runStateT, evalStateT)
import Prelude hiding (filter)
import qualified System.IO.Error as IO
import qualified System.IO as IO

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

-- | Run the parser
parse :: Parser t a -> [t] -> Maybe a
parse = evalStateT

parseToEither :: Parser t a -> [t] -> Either String a
parseToEither p s = case parse p s of
  Just x -> Right x
  Nothing -> Left "No parses"

parseFromFile :: Parser Char a -> FilePath -> IO (Either String a)
parseFromFile parser filename = do
  IO.catchIOError
    (do
        handle <- IO.openFile filename IO.ReadMode
        str <- IO.hGetContents handle
        pure $ parseToEither parser str)
    (return . Left . show)