{-# LANGUAGE LambdaCase #-}

module Parser where

import Control.Monad (guard)
import Control.Monad.State (StateT (StateT), runStateT, evalStateT)
import Prelude hiding (filter)

type Parser t a = StateT [t] Maybe a

getTok :: Parser t t
getTok = StateT $ \case
  (c : cs) -> Just (c, cs)
  [] -> Nothing

filter :: (a -> Bool) -> Parser t a -> Parser t a
filter f p = StateT $ \s -> do
  (c, cs) <- runStateT p s
  guard (f c)
  return (c, cs)

satisfy :: (t -> Bool) -> Parser t t
satisfy p = filter p getTok

token :: Eq t => t -> Parser t t
token t = satisfy (t ==)

tokens :: Eq t => [t] -> Parser t [t]
tokens = foldr (\t p -> (:) <$> token t <*> p) (pure [])

parse :: Parser t a -> [t] -> Maybe a
parse = evalStateT