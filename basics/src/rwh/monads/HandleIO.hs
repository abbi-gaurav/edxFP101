{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module HandleIO
  (
    HandleIO
  , Handle
  , IOMode(..)
  , runHandleIO
  , openFile
  , hClose
  , hPutStrLn
  ) where

import           Control.Monad.Trans (MonadIO (..))
import           System.Directory
import           System.IO           (Handle, IOMode (..))
import qualified System.IO

newtype HandleIO a = HandleIO {
  runHandleIO :: IO a
  } deriving (Functor, Applicative, Monad)

openFile :: FilePath -> IOMode -> HandleIO Handle
openFile path mode = HandleIO (System.IO.openFile path mode)

hClose :: Handle -> HandleIO ()
hClose = HandleIO . System.IO.hClose

hPutStrLn :: Handle -> String -> HandleIO ()
hPutStrLn h s = HandleIO (System.IO.hPutStrLn h s)

safeHello :: FilePath -> HandleIO ()
safeHello path = do
  h <- openFile path WriteMode
  hPutStrLn h "Hello World"
  hClose h

instance MonadIO HandleIO where
  liftIO = HandleIO

tidyHello :: FilePath -> HandleIO()
tidyHello path = do
  safeHello path
  liftIO (removeFile path)
