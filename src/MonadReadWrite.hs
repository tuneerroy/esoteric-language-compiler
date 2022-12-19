module MonadReadWrite where

import Control.Monad (unless)
import Control.Monad.Except (ExceptT)
import Control.Monad.State (StateT)
import Control.Monad.Trans (MonadTrans (lift))

-- | A typeclass that represents being able to perform IO
-- | More general so we can use pure alternatives
class Monad m => MonadReadWrite m where
  readChar :: m Char
  writeString :: String -> m ()

-- | For most purposes, we want actual IO to be our MonadReadWrite
instance MonadReadWrite IO where
  readChar :: IO Char
  readChar = getChar
  writeString :: String -> IO ()
  writeString = putStrLn

instance MonadReadWrite m => MonadReadWrite (ExceptT e m) where
  readChar :: ExceptT e m Char
  readChar = lift readChar
  writeString :: String -> ExceptT e m ()
  writeString = lift . writeString

instance MonadReadWrite m => MonadReadWrite (StateT s m) where
  readChar :: StateT s m Char
  readChar = lift readChar
  writeString :: String -> StateT s m ()
  writeString = lift . writeString

readLine :: MonadReadWrite m => m String
readLine = do
  c <- readChar
  case c of
    '\n' -> return ""
    _ -> (c :) <$> readLine
