{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE TypeSynonymInstances       #-}

module WriterIO where

import           Control.Monad.Writer
import           MonadHandle
import           SafeHello
import           System.IO            (IOMode (..))

data Event = Open FilePath IOMode
           | Put String String
           | Close String
           | GetContents String
           deriving (Show)

newtype WriterIO a = W { runW :: Writer[Event] a }
                   deriving (Applicative, Functor, Monad, MonadWriter [Event])

runWriterIO :: WriterIO a -> (a, [Event])
runWriterIO = runWriter . runW

instance MonadHandle FilePath WriterIO where
  openFile path mode = tell [Open path mode] >> return path
  hPutStr h str = tell [Put h str]
  hClose h = tell [Close h]
  hGetContents h = tell [GetContents h] >> return ""
